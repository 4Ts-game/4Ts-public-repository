% -*- Mode: Prolog -*-

:- module(kbAdvanced,
          [checkBoard_advanced/5, valid_ttt_pattern/4, checkInconsistencies/5, rebindSuggestionSlot/4]).

:- use_module(kbCommons).
:- use_module(kbBase).
:- use_module(suggestionsAdvanced, [checkBoard_advanced/5, provideSuggestions_advanced/2]).

:- dynamic(valid_ttt_pattern/4).

/**********
  * checkBoard_advanced (+board,
  *                      -inconsistentSlots,
  *                      -inconsistentDetails,
  *                      -missingSlots,
  *                      -suggestionPairs)
  *
*/

checkBoard_advanced(board(_, _, ListOfColumns), Inconsistencies, InconsistencyDetails, [], []) :-
  kbCommons:wrongCardTypes(ListOfColumns, Inconsistencies, InconsistencyDetails),!. 
checkBoard_advanced(Board, Inconsistencies, InconsistencyDetails, MissingSlots, []) :-
  board(_, true, Columns) = Board,
  %debug_format('checking inconsistencies and missing slots~n', []),
  checkInconsistencies(false, Columns, [], Inconsistencies, InconsistencyDetails),
  removeSuggestionCard(Columns, ColumnsNoSuggestionCards),
  checkMissingSlots(ColumnsNoSuggestionCards, MissingSlots),
  debug_format('checkInconsistencies reported ~n~p~n~p~n', [Inconsistencies, InconsistencyDetails]),
  debug_format('checkMissingSlots reported ~n~p~n', [MissingSlots]), !.
checkBoard_advanced(Board, Inconsistencies, InconsistencyDetails, [], Suggestions) :-
  board(_, false, Columns) = Board,
  %debug_format('checking inconsistencies - but not missing slots~n', []),
  (provideSuggestions_advanced(Columns, Suggestions),
    Suggestions = [_|_],  % suggestions reported
    Inconsistencies = [], InconsistencyDetails = [];
    rebindSuggestionSlot(SuggestionVar, _, Columns, CleanColumns),  % if no suggestions reported (not asked, or not possible), check for current inconsistencies
    checkInconsistencies(false(SuggestionVar), CleanColumns, [], Inconsistencies, InconsistencyDetails),
    Suggestions = []
  ),
  debug_format('checkInconsistencies and suggestions reported ~n~p~n~p~n~p~n', [Inconsistencies, InconsistencyDetails, Suggestions]), !.
checkBoard_advanced(_, [], [], [], []) :-
  debug_format('why am I here?~n', []).


/*
  * checkInconsistencies (+ForSuggestions, +Columns, [], -inconsistentSlots, -incosistentSlotDetails)
  *
  * Checks if the current set of columns is "valid", even if possibly incomplete
*/
checkInconsistencies(_, [], _, [], []).
checkInconsistencies(ForSuggestions, [Col|NextColumns], PreviousColumns, Result, ResultDetails) :-
  %debug_format('unbinding ~p~n', [Col]),
  unbindSlots([Col], [column(ColN, _, Task1, Team1, Tech1_1, Tech1_2, Task2, Team2, Tech2_1, Tech2_2)]),  % defined in kbBase
  %debug_format('unbound as ~p~n', [column(ColN, _, Task1, Team1, Tech1_1, Tech1_2, Task2, Team2, Tech2_1, Tech2_2)]),
  (empty(Task1, Team1, Tech1_1, Tech1_2),
    %debug_format('column ~p section A is empty; ignoring it~n', [ColN]),
    NewPreviousColumns1 = PreviousColumns,
    Result1a = [], Result1aDetails = [],
    Result1b = [], Result1bDetails = [],
    Result1c = [], Result1cDetails = [],
    Result1d = [], Result1dDetails = [],
    Result1e = [], Result1eDetails = [],
    DurationsA = [0,1];
    % top part of ColN contains some data
    %debug_format('column ~p section A: processing inconsistencies~n', [ColN]),
    addTask(Task2, PreviousColumns, PreviousColumnsPlus),  % if section B contains a task, include it in the prerequisites checks of section A
    getPrerequisiteErrors(Task1, PreviousColumnsPlus, ColN, 'A', Result1a, Result1aDetails), % check prerequisites
    getInvalidTeamTechCouplings(Team1, Tech1_1, ColN, 'A', 1, Result1b, Result1bDetails), % check team/tech incompatibilities on tech1
    getIncompatibleTechs(Tech1_1, Tech1_2, ColN, 'A', Result1c, Result1cDetails),         % check mismatching techs used in the same context
    getInvalidTeamTechCouplings(Team1, Tech1_2, ColN, 'A', 2, Result1d, Result1dDetails), % check team/tech incompatibilities on tech2
    (checkIfBothOneWeekTasks(Task1, Task2, ColN, Result1e, Result1eDetails);
      getInvalidPatternErrors(Task1, Team1, Tech1_1, Tech1_2, ColN, 'A', _, DurationsA, Result1e, Result1eDetails)  % check if there is any matching pattern
    ),
    addTask(Task1, PreviousColumns, NewPreviousColumns1) % keep chain of tasks to check prerequisites
  ),
  (empty(Task2, Team2, Tech2_1, Tech2_2),
    %debug_format('column ~p section B is empty; ignoring it~n', [ColN]),
    NewPreviousColumns2 = NewPreviousColumns1,
    Result2a = [], Result2aDetails = [],
    Result2b = [], Result2bDetails = [],
    Result2c = [], Result2cDetails = [],
    Result2d = [], Result2dDetails = [],
    Result2e = [], Result2eDetails = [];
    % bottom part of ColN contains some data; same checks as for top part
    %debug_format('column ~p section B: processing inconsistencies~n', [ColN]),
    getPrerequisiteErrors(Task2, NewPreviousColumns1, ColN, 'B', Result2a, Result2aDetails),
    getInvalidTeamTechCouplings(Team2, Tech2_1, ColN, 'B', 1, Result2b, Result2bDetails),
    getIncompatibleTechs(Tech2_1, Tech2_2, ColN, 'B', Result2c, Result2cDetails),
    getInvalidTeamTechCouplings(Team2, Tech2_2, ColN, 'B', 2, Result2d, Result2dDetails),
    getInvalidPatternErrors(Task2, Team2, Tech2_1, Tech2_2, ColN, 'B', DurationsA, _, Result2e, Result2eDetails),
    addTask(Task2, NewPreviousColumns1, NewPreviousColumns2)
  ),
  checkInconsistencies(ForSuggestions, NextColumns, NewPreviousColumns2, ResultNext, ResultDetailsNext),
  append([Result1a, Result1b, Result1c, Result1d, Result1e, Result2a, Result2b, Result2c, Result2d, Result2e, ResultNext], ResultTmp),
  append([Result1aDetails, Result1bDetails, Result1cDetails, Result1dDetails, Result1eDetails, Result2aDetails, Result2bDetails, Result2cDetails, Result2dDetails, Result2eDetails, ResultDetailsNext], ResultDetailsTmp),
  remove_duplicates(ResultTmp, Result, ResultDetailsTmp, ResultDetails),  % drops less specific errors for the same slot
  !.

/*
  * checkIfBothOneWeekTasks(+Task1, +Task2, +ColN, -Slots, -Details)
  *
  * Checks if two tasks (in the same column) have both one week duration and it returns the corresponding error
*/
checkIfBothOneWeekTasks(Task1, Task2, _, _, _) :-
  (var(Task1); var(Task2)),
  !, fail.
checkIfBothOneWeekTasks(Task1, Task2, _, _, _) :-
  (kbAdvanced:valid_ttt_pattern(Task1, _, _, 0); kbAdvanced:valid_ttt_pattern(Task2, _, _, 0)),
  !, fail.
checkIfBothOneWeekTasks(_, _, ColN, [Slot], [error('007', Details)]) :-
  % debug_format("checkIfBothOneWeekTasks(~p, ~p, ~p, ...~n", [Task1, Task2, ColN]),
  format(atom(Slot), 'C~d-ABB-CSS-TASK', [ColN]), !,
  error_details('007', Details), !,
  !.


/*
  * checkMissingSlots (+Columns, -missingSlots)
  *
  * Checks if the current set of columns is missing slots
*/
checkMissingSlots([], []).
checkMissingSlots([Col|NextColumns], Result) :-
  %debug_format('unbinding ~p~n', [Col]),
  unbindSlots([Col], [column(ColN, _, Task1, Team1, Tech1_1, Tech1_2, Task2, Team2, Tech2_1, Tech2_2)]),
  (empty(Task1, Team1, Tech1_1, Tech1_2), !,
    % top part of ColN is empty
    (empty(Task2, Team2, Tech2_1, Tech2_2), !,
      % bottom part of ColN is empty too
      Result1 = [],
      Result2 = [];
      % bottom part of ColN is not empty
      getEmptySlotErrors(Task2, Team2, Tech2_1, Tech2_2, ColN, 'B', Result2),
      (Result2 = [],
        % no missing slots reported for bottom part
        Result1 = [];
        % bottom part has missing slots; add the ones at the top too
        format(atom(SlotTaskID), 'C~d-ABA-CSS-TASK', [ColN]),
        format(atom(SlotTeamID), 'C~d-ABA-CSS-TEAM', [ColN]),
        format(atom(SlotTech1ID), 'C~d-ABA-CSS-TEC1', [ColN]),
        task(SlotTaskCardID, _),
        team(SlotTeamCardID, _),
        technology(SlotTech1CardID, _),
        Result1 = [slotCardPair(SlotTaskID, SlotTaskCardID), slotCardPair(SlotTeamID, SlotTeamCardID), slotCardPair(SlotTech1ID, SlotTech1CardID)]
      )
    );
    % top part of ColN is not empty
    getEmptySlotErrors(Task1, Team1, Tech1_1, Tech1_2, ColN, 'A', Result1),
    getEmptySlotErrors(Task2, Team2, Tech2_1, Tech2_2, ColN, 'B', Result2)
  ),
  checkMissingSlots(NextColumns, ResultNext),
  append([Result1, Result2, ResultNext], Result), !.

/* rebindSuggestionSlot:
  * replaces the call-for-suggestion card with either a variable or an atom
  * it returns the type of the slot where the suggestion card is (task, team or technology)
  * if no suggestion card is present, the suggestionType is returned not instantiated
*/
rebindSuggestionSlot(_, _, [], []).
rebindSuggestionSlot(SuggestionVar, SuggestionType, [Col | Cols], [UCol | Cols]) :-
  Col =.. LCol,
  controlCard(SuggestionCard, 'call-for-suggestion'),
  append([column,_|Before],[SuggestionCard|_],LCol),
  length(Before, Pos),
  (Pos == 1,  %task
    SuggestionType = task;
   Pos == 2,  %team
    SuggestionType = team;
   SuggestionType = technology
  ),
  rebindSuggestionColumn(SuggestionVar, LCol, ULCol),
  UCol =.. ULCol, !.
rebindSuggestionSlot(SuggestionVar, SugestionType, [Col | Cols], [Col | ULCols]) :-
  rebindSuggestionSlot(SuggestionVar, SugestionType, Cols, ULCols).

rebindSuggestionColumn(_, [], []).
rebindSuggestionColumn(SuggestionVar, [SuggestionCard | Rest], [SuggestionVar | Rest]) :-
  controlCard(SuggestionCard, 'call-for-suggestion').
rebindSuggestionColumn(SuggestionVar, [Val | Rest], [Val | Rest1]) :-
  rebindSuggestionColumn(SuggestionVar, Rest, Rest1).


/* getEmptySlotErrors
  * builds a list of missing slots
*/
getEmptySlotErrors(Task, Team, Tech1, Tech2, _ColN, _ColSection, []) :-
  atom(Task), atom(Team), atom(Tech1), atom(Tech2), !.
getEmptySlotErrors(Task, Team, Tech1, Tech2, _ColN, _ColSection, []) :-
  var(Task), var(Team), var(Tech1), var(Tech2), !.
getEmptySlotErrors(Task, Team, Tech1, Tech2, ColN, ColSection, Slots) :-
  var(Task), !,
    format(atom(SlotID), 'C~d-AB~s-CSS-TASK', [ColN, ColSection]),
    task(SlotCardID, _),
    getEmptySlotErrors('', Team, Tech1, Tech2, ColN, ColSection, OtherSlots),
    append(OtherSlots, [slotCardPair(SlotID, SlotCardID)], Slots);
    % Task is instantiated
    fail.
getEmptySlotErrors(Task, Team, Tech1, Tech2, ColN, ColSection, Slots) :-
  var(Team), !,
    format(atom(SlotID), 'C~d-AB~s-CSS-TEAM', [ColN, ColSection]),
    team(SlotCardID, _),
    getEmptySlotErrors(Task, '', Tech1, Tech2, ColN, ColSection, OtherSlots),
    append(OtherSlots, [slotCardPair(SlotID, SlotCardID)], Slots);
    % Team is instantiated
    fail.
getEmptySlotErrors(Task, Team, Tech1, Tech2, ColN, ColSection, Slots) :-
  (var(Tech1), var(Tech2)), !,
    % No technologies spefified
    format(atom(SlotID), 'C~d-AB~s-CSS-TEC1', [ColN, ColSection]),
    technology(SlotCardID, _),
    Slots = [slotCardPair(SlotID, SlotCardID)];
    % At least one tech is specified; we need to check if patterns for these combinations require two
    (var(Tech1),
      %Tech1 is not specified, Tech2 is
      (kbAdvanced:valid_ttt_pattern(Task, Team, ['', Tech2], _),
        Slots = [];
        % Not valid without Tech1 specified
        (kbAdvanced:valid_ttt_pattern(Task, Team, [_, Tech2], _),
          format(atom(SlotID), 'C~d-AB~s-CSS-TEC1', [ColN, ColSection]),
          technology(SlotCardID, _),
          Slots = [slotCardPair(SlotID, SlotCardID)];
          % No good patterns anyway
          Slots = []
        )
      );
      %Tech2 is not specified, Tech1 is
      (kbAdvanced:valid_ttt_pattern(Task, Team, [Tech1, ''], _),
        Slots = [];
        % Not valid without Tech2 specified
        (kbAdvanced:valid_ttt_pattern(Task, Team, [Tech1, _], _),
          format(atom(SlotID), 'C~d-AB~s-CSS-TEC2', [ColN, ColSection]),
          technology(SlotCardID, _),
          Slots = [slotCardPair(SlotID, SlotCardID)];
          % No good patterns anyway
          Slots = []
        )
      )
    ), !.
  
/* getPrerequisiteErrors
  * builds a list of errors caused by task used without the proper prerequisites in previous columns
*/
getPrerequisiteErrors(Task, _, _, _, [], []) :-
  var(Task), !.
getPrerequisiteErrors(Task, PreviousColumns, _ColN, _ColSection, [], []) :-
  checkPrerequisites(Task, PreviousColumns),
  %debug_format('prerequistes check passed for task ~p in column ~p (~s)~n', [Task, _ColN, _ColSection]),
  !.
getPrerequisiteErrors(_Task, _, ColN, ColSection, [Slot], [error('001', Details)]) :-
  %debug_format('prerequisites check failed for task ~p in column ~p (~s)~n', [_Task, ColN, ColSection]),
  format(atom(Slot), 'C~d-AB~s-CSS-TASK', [ColN, ColSection]),
  error_details('001', Details),
  !.

/* getInvalidPatternErrors
  * builds a list of slots and details about mis-matched patterns
*/
getInvalidPatternErrors(TaskOrig, TeamOrig, Tech1Orig, Tech2Orig, ColN, ColSection, PrevDurations, AvailableDurations, Slots, Details) :-
  copy_term(TaskOrig, Task), copy_term(TeamOrig, Team), copy_term(Tech1Orig, Tech1), copy_term(Tech2Orig, Tech2), !,  % don't instantiate the incoming arguments
  %debug_format('getInvalidPatternErrors: ~n~p~n~p~n', [getInvalidPatternErrors(TaskOrig, TeamOrig, Tech1Orig, Tech2Orig, ColN, ColSection, Slots, Details), getInvalidPatternErrorsImpl(Task, Team, Tech1, Tech2, ColN, ColSection, Slots, Details)]),
  getInvalidPatternErrorsImpl(Task, Team, Tech1, Tech2, ColN, ColSection, PrevDurations, AvailableDurations, Slots, Details).

getInvalidPatternErrorsImpl(Task, Team, Tech1, Tech2, _, _, _, [0,1], [], []) :-
  empty(Task, Team, Tech1, Tech2), !.
getInvalidPatternErrorsImpl(Task, Team, Tech1, Tech2, _ColN, 'A', _, [0,1], [], []) :-
  kbAdvanced:valid_ttt_pattern(Task, Team, [Tech1, Tech2], 0),
  kbAdvanced:valid_ttt_pattern(Task, Team, [Tech1, Tech2], 1),
  %debug_format('consistency check passed for column ~p top:~n~p~n', [_ColN, valid_ttt_pattern(Task, Team, [Tech1, Tech2], _D)]),
  !.
getInvalidPatternErrorsImpl(Task, Team, Tech1, Tech2, _ColN, 'A', _, [1], [], []) :-
  not(kbAdvanced:valid_ttt_pattern(Task, Team, [Tech1, Tech2], 0)),
  kbAdvanced:valid_ttt_pattern(Task, Team, [Tech1, Tech2], 1),
  %debug_format('consistency check passed for column ~p top:~n~p~n', [_ColN, valid_ttt_pattern(Task, Team, [Tech1, Tech2], _D)]),
  !.
getInvalidPatternErrorsImpl(Task, Team, Tech1, Tech2, _ColN, 'A', _, [0], [], []) :-
  kbAdvanced:valid_ttt_pattern(Task, Team, [Tech1, Tech2], 0),
  not(kbAdvanced:valid_ttt_pattern(Task, Team, [Tech1, Tech2], 1)),
  %debug_format('consistency check passed for column ~p top:~n~p~n', [_ColN, valid_ttt_pattern(Task, Team, [Tech1, Tech2], _D)]),
  !.
  
getInvalidPatternErrorsImpl(Task, Team, Tech1, Tech2, ColN, 'A', _, [], [Slot], [error('002', Detail)]) :-
  %debug_format('consistency check failed for column ~p top~n', [ColN]),
  mostSpecificInconsistentItem(Task, Team, Tech1, Tech2, _, SuspiciousItem),
  format(atom(Slot), 'C~d-ABA-CSS-~s', [ColN, SuspiciousItem]),
  error_details('002', Detail),
  !.
getInvalidPatternErrorsImpl(Task, Team, Tech1, Tech2, _ColN, 'B', [_,_], _, [], []) :-
  kbAdvanced:valid_ttt_pattern(Task, Team, [Tech1, Tech2], _D),
  %debug_format('consistency check passed for column ~p bottom:~n~p~n', [_ColN, valid_ttt_pattern(Task, Team, [Tech1, _Tech2], 0)]),
  !.
getInvalidPatternErrorsImpl(Task, Team, Tech1, Tech2, _ColN, 'B', [1], _, [], []) :-
  kbAdvanced:valid_ttt_pattern(Task, Team, [Tech1, Tech2], 0),
  %debug_format('consistency check passed for column ~p bottom:~n~p~n', [_ColN, valid_ttt_pattern(Task, Team, [Tech1, _Tech2], 0)]),
  !.
getInvalidPatternErrorsImpl(Task, Team, Tech1, Tech2, _ColN, 'B', [0], _, [], []) :-  % if previous duration is 0, we accept both in section B
  kbAdvanced:valid_ttt_pattern(Task, Team, [Tech1, Tech2], _),
  %debug_format('consistency check passed for column ~p bottom:~n~p~n', [_ColN, valid_ttt_pattern(Task, Team, [Tech1, _Tech2], 0)]),
  !.
getInvalidPatternErrorsImpl(Task, Team, Tech1, Tech2, ColN, 'B', _, _, [Slot], [error('003', Detail)]) :-
  %debug_format('consistency check failed for column ~p bottom; try with one week duration~n', [ColN]),
  % if the task is fully specified, and there is a pattern matching it for one week duration, provide a more specific error
  (atom(Task), atom(Team), (atom(Tech1); atom(Tech2)), kbAdvanced:valid_ttt_pattern(Task, Team, [Tech1, Tech2], 1)), !,
  % there is a valid pattern for a one week duration
  format(atom(Slot), 'C~d-ABB-CSS-~s', [ColN, 'TASK']),
  error_details('003', Detail),
  !.
getInvalidPatternErrorsImpl(Task, Team, Tech1, Tech2, ColN, 'B', _, _, [Slot], [error('002', Detail)]) :-
  %debug_format('consistency check failed for column ~p bottom~n', [ColN]),
  mostSpecificInconsistentItem(Task, Team, Tech1, Tech2, 0, SuspiciousItem),
  format(atom(Slot), 'C~d-ABB-CSS-~s', [ColN, SuspiciousItem]),
  error_details('002', Detail),
  !.

/* getInvalidTeamTechCouplings
  * builds a list of slots and details about incompatible team/technology matches
*/
getInvalidTeamTechCouplings(Team, Tech, _ColN, _ColSection, _TechIndex, [], []) :-
  (var(Team); var(Tech)), !.
getInvalidTeamTechCouplings(Team, Tech, ColN, ColSection, TechIndex, Slots, Details) :-
  valid_techs_for_team(Team, GoodTechs), !,
  (member(Tech, GoodTechs),
    Slots = [], Details = [];
    format(atom(Slot), 'C~d-AB~s-CSS-TEC~d', [ColN, ColSection, TechIndex]),
    Code = '004',
    error_details(Code, Detail),
    Slots = [Slot], Details = [error(Code, Detail)]
  ), !.

/* getIncompatibleTechs
  * builds a list of slots and details about mis-matching technologies used in the same task
*/
getIncompatibleTechs(Tech1, Tech2, _, _, [], []) :-
  (var(Tech1); var(Tech2)), !.
getIncompatibleTechs(Tech1, Tech2, ColN, ColSection, [Slot], [error(Code, Details)]) :-
  incompatible_techs(Tech1, Techs),
  member(Tech2, Techs),
  %debug_format('compatible techs check failed for ~p and ~p in column ~p (~s)', [Tech1, Tech2, ColN, ColSection]),
  format(atom(Slot), 'C~d-AB~s-CSS-TEC2', [ColN, ColSection]),
  (Tech1 = Tech2,
    Code='005', error_details(Code, Details);
    Code='006', error_details(Code, Details)
  ), !.
getIncompatibleTechs(_, _, _, _, [], []).


addTask(Task, Columns, Columns) :-
  var(Task), !.
addTask(Task, InColumns, OutColumns) :-
  append(InColumns, [Task], OutColumns).

checkPrerequisites(Task, _PreviousTasks) :-
  var(Task), !.
checkPrerequisites(Task, PreviousTasks) :-
  prerequisite_tasks(Task, PrerequisiteTasks),
  (
    PrerequisiteTasks = []; % no prerequisites; check succeeds
    % prerequisites defined: verify at least one is satisfied
    member(PrerequisiteTask, PrerequisiteTasks),
    member(PrerequisiteTask, PreviousTasks)
  ), !.

empty(A, B, C, D) :-
  var(A), var(B), var(C), var(D).

mostSpecificInconsistentItem(Task, _, _, _, Duration, Result) :-  % Catch tasks which are never synchronous here (just for speed)
  not(kbAdvanced:valid_ttt_pattern(Task, _, _, Duration)), !,
  Result = 'TASK'.
mostSpecificInconsistentItem(Task, Team, Tech1, Tech2, Duration, Result) :-
  atom(Tech2), !,
  (kbAdvanced:valid_ttt_pattern(Task, Team, [Tech1, _], Duration),
    Result = 'TEC2';
    mostSpecificInconsistentItem(Task, Team, Tech1, _, Duration, Result)
  ).
mostSpecificInconsistentItem(Task, Team, Tech1, _, Duration, Result) :-
  atom(Tech1), !,
  (kbAdvanced:valid_ttt_pattern(Task, Team, _, Duration),
    Result = 'TEC1';
    mostSpecificInconsistentItem(Task, Team, _, _, Duration, Result)
  ).
mostSpecificInconsistentItem(Task, Team, _, _, Duration, Result) :-
  atom(Team), !,
  (kbAdvanced:valid_ttt_pattern(Task, _, _, Duration),
    Result = 'TEAM';
    Result = 'TASK'
  ).
mostSpecificInconsistentItem(_, _, _, _, _, Result) :-
  Result = 'TASK'.


/* ttt/3 contains all the valid task/team/technology patterns
*/

% Writing a report
ttt('101', or(['201', '202', '203']), or(['308', '304']), 1).                 % text editor or wiki
ttt('101', or(['201', '202', '203']), and([or(['308', '304']), '301']), 1).   % (text editor or wiki) and forum
ttt('101', or(['201', '202', '203']), and([or(['308', '304']), '310']), 1).   % (text editor or wiki) and no tech
% Studying
ttt('102', or(['201', '202', '203']), '306', 1).
% Finding materials
ttt('103', or(['201', '202', '203']), '307', 1).
% Preparing a list
ttt('104', or(['201', '202', '203', '204', '205', '206']), '308', 1).         % text editor
ttt('104', or(['201', '202', '203', '204', '205', '206']), '301', 1).         % forum
ttt('104', or(['201', '202', '203', '204', '205', '206']), and(['308', '301']), 1).   % text editor and forum
ttt('104', or(['201', '202', '203', '204', '205', '206']), and(['308', '305']), 0).   % text editor and videoconference
ttt('104', or(['201', '202', '203', '204', '205', '206']), and(['308', '310']), 0).   % text editor and no tech
% Commenting on someone else's work
ttt('105', or(['201', '202', '203']), or(['308','304']), 1).                  % text editor or wiki
ttt('105', or(['201', '202', '203']), and(['301', or(['308', '304'])]), 1).   % (text editor or wiki) and forum
ttt('105', or(['201', '202', '203']), and(['310', or(['308', '304'])]), 1).   % (text editor or wiki) and no tech
ttt('105', or(['201', '202', '203']), '301', 1).                              % forum
% Preparing a presentation
ttt('106', or(['201', '202', '203', '204']), and(['302', or(['301', '303', '310'])]), 1).
% Carrying out an assignment
ttt('107', or(['201', '202', '203']), and(['311', or(['301', '303', '310'])]), 1).
% Giving a presentation
ttt('108', '206', and(['309', '310']), 0).      % Projector and no tech
ttt('108', '206', '303', 0).                    % IWB
ttt('108', '206', '305', 0).                    % Video conference
% Solving a problem
ttt('109', or(['201', '202', '203', '206']), and(['308', '310']), 0).   % text editor and no tech
ttt('109', or(['201', '202', '203', '206']), and(['308', '305']), 0).   % text editor and videoconference
ttt('109', or(['201', '202', '203', '206']), '301', 1).                 % forum
ttt('109', or(['201', '202', '203', '206']), and(['308', '301']), 1).   % text editor and forum
ttt('109', or(['201', '202', '203', '206']), '308', 1).                 % text editor
% Interviewing an expert
ttt('110', or(['204', '205', '206']), '310', 0).                        % no tech
ttt('110', or(['204', '205', '206']), '305', 0).                        % videoconference
ttt('110', or(['204', '205', '206']), '301', 1).                        % forum
% Assuming roles
ttt('111', '203', '301', 1).                          % forum
ttt('111', '203', '305', 0).                          % videoconference
ttt('111', '203', '310', 0).                          % no tech
% Producing an artifact
ttt('112', or(['201', '202', '203', '204', '205', '206']), and(['311', or(['303', '301', '310'])]), 1).
% Debating
ttt('113', or(['202', '203', '204', '205', '206']), '301', 1).    % forum
ttt('113', or(['202', '203', '204', '205', '206']), '305', 0).    % videoconference
ttt('113', or(['202', '203', '204', '205', '206']), '310', 0).    % no tech


/* expand
  * expands ttt patterns resolving or/and predicates for teams and technologies
*/
expand(ttt(Task, or(Teams), Technologies, D)) :-
  !, member(Team, Teams),
  expand(ttt(Task, Team, Technologies, D)).
expand(ttt(Task, Team, or(Technologies), D)) :-
  !, member(Technology, Technologies),
  expand(ttt(Task, Team, Technology, D)).
expand(ttt(Task, Team, and([or(OrTechs), AndTech]), D)) :-
  !, member(OrTech, OrTechs),
  assert_ttt_expanded(Task, Team, [OrTech, AndTech], D).
expand(ttt(Task, Team, and([AndTech, or(OrTechs)]), D)) :-
  !, member(OrTech, OrTechs),
  assert_ttt_expanded(Task, Team, [OrTech, AndTech], D).
expand(ttt(Task, Team, and([Tech1, Tech2]), D)) :-
  !, assert_ttt_expanded(Task, Team, [Tech1, Tech2], D).
expand(ttt(Task, Team, Tech, D)) :-
  assert_ttt_expanded(Task, Team, [Tech, ''], D).

assert_ttt_expanded(Task, Team, [Tech1, Tech2], D) :-
  %debug_format('Asserting ~p and ~p~n', [valid_ttt_pattern(Task, Team, [Tech1, Tech2], D), valid_ttt_pattern(Task, Team, [Tech2, Tech1], D)]),
  assert(kbAdvanced:valid_ttt_pattern(Task, Team, [Tech1, Tech2], D)),
  assert(kbAdvanced:valid_ttt_pattern(Task, Team, [Tech2, Tech1], D)), !.
  
expand_all_TTTs :-
  retractall(kbAdvanced:valid_ttt_pattern(_,_,_)),
  ttt(A, B, C, D),
  expand(ttt(A, B, C, D)),
  fail.
expand_all_TTTs.

% Constraints when first T (task) is missing
% Individual learners
valid_techs_for_team('201', ['302', '304', '306', '307', '308', '311']).
% Pairs
valid_techs_for_team('202', ['301', '302', '303', '304', '305', '306', '307', '308', '309', '310', '311']).
% Small groups
valid_techs_for_team('203', ['301', '302', '303', '304', '305', '306', '307', '308', '309', '310', '311']).
% Medium groups
valid_techs_for_team('204', ['301', '302', '303', '304', '305', '306', '307', '308', '309', '310', '311']).
% Large groups
valid_techs_for_team('205', ['302', '303', '304', '305', '306', '307', '308', '309', '310', '311']).
% Plenary
valid_techs_for_team('206', ['301', '302', '303', '304', '305', '309', '310']).


% Incompatible adjacent technologies
% Forum
incompatible_techs('301', ['301', '305', '310']).
% Presentation software
incompatible_techs('302', ['302']).
% Interactive whiteboard
incompatible_techs('303', ['303']).
% Wiki software
incompatible_techs('304', ['304']).
% Video conference
incompatible_techs('305', ['301', '305', '309', '310']).
% Selected study material
incompatible_techs('306', ['306']).
% Source of materials
incompatible_techs('307', ['307']).
% Text editor
incompatible_techs('308', ['308']).
% Projector
incompatible_techs('309', ['305', '309']).
% No technology
incompatible_techs('310', ['301', '305', '310']).
% Materials and tools
incompatible_techs('311', ['311']).

% Task prerequisites
% Writing a text
prerequisite_tasks('101', []).
% Studying
prerequisite_tasks('102', []).
% Finding materials
prerequisite_tasks('103', []).
% Preparing a list of questions
prerequisite_tasks('104', []).
% Commenting on someone's work
prerequisite_tasks('105', ['101', '104', '106', '107', '109', '112']).
% Preparing a presentation
prerequisite_tasks('106', []).
% Carrying out an assignment
prerequisite_tasks('107', []).
% Giving a presentation
prerequisite_tasks('108', ['101', '106', '107', '109', '112']).
% Solving a problem
prerequisite_tasks('109', []).
% Interviewing an expert
prerequisite_tasks('110', ['104']).
% Assuming roles
prerequisite_tasks('111', []).
% Producing an artifact
prerequisite_tasks('112', []).
% Debating
prerequisite_tasks('113', []).


% utilities
remove_duplicates(InSlots, OutSlots, InDetails, OutDetails) :-
  remove_duplicates_impl([], InSlots, OutSlots, InDetails, OutDetails).

remove_duplicates_impl(_, [], [], [], []).
remove_duplicates_impl(Included, [Head | Tail], Result, [_ | DetailsTail], DetailsResult) :-
    member(Head, Included), !,
    remove_duplicates_impl(Included, Tail, Result, DetailsTail, DetailsResult).
remove_duplicates_impl(Included, [Head | Tail], [Head | Result], [DetailsHead | DetailsTail], [DetailsHead | DetailsResult]) :-
    remove_duplicates_impl([Head | Included], Tail, Result, DetailsTail, DetailsResult).

:- expand_all_TTTs.


