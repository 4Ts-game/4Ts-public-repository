% -*- Mode: Prolog -*-

:- module(kbCommons,
          [card_instances_commons/2,
           toCardType/2,
           task/2,
           isTask/1,
           team/2,
           isTeam/1,
           technology/2,
           isTechnology/1,
           controlCard/2,
		   debug_writeln/1,
           debug_format/2,
           is_empty/1,
           is_not_empty/1,
		   is_empty_column/1,
           not_both_empty/2,
           non_empty_slot_in_col/2,
		   qualifiedSlotNames/2,
		   unqualifiedSlotNames/1,
           taskPrerequisites/2,
           taskSlotPairSequence/2,
           removeSuggestionCard/2,
           unbindSlotsWithSuggestion/4,
           error_details/2,
           convertSlotNameIntoGenericError/2]).

:- use_module(kbBase, [card_instances/2, isTechnique/1]).

/*
  *
  * Card reference
  *
  * https://docs.google.com/spreadsheets/d/1vbKRP_QAa5aDEWWEEgJravq-R_m_ZlHpkclr3hm7-lw/edit?usp=drive_web&ouid=113162760941048132931
  *
*/

%%%
%%% card_instances_commons/2: type_id, list_of_instance_ids
%%% these cards are available both in level base and in level advanced
%%%

card_instances_commons('101', ['32', '33', '34', '35']). % Writing a text
card_instances_commons('102', ['114', '115', '116', '117']). % Studying
card_instances_commons('103', ['118', '119', '120', '121']). % Finding materials
card_instances_commons('104', ['122', '123', '124', '125']). % Preparing a lis of questions
card_instances_commons('105', ['126', '127', '128', '129']). % Commenting on someone's work
card_instances_commons('106', ['130', '131', '132', '133']). % Preparing a presentation
card_instances_commons('107', ['134', '135', '136', '137']). % Carrying out an assignment
card_instances_commons('108', ['138', '139', '140', '141']). % Giving a presentation
card_instances_commons('109', ['142', '143', '144', '145']). % Solving a problem
card_instances_commons('110', ['146', '147', '148', '149']). % Interviewing an expert
card_instances_commons('111', ['150', '151', '152', '153']). % Assuming roles
card_instances_commons('112', ['154', '155', '156', '157']). % Producing an artefact
card_instances_commons('113', ['158', '159', '160', '161']). % Debating

card_instances_commons('201', ['24', '25', '26', '27']). % Individual learners
card_instances_commons('202', ['58', '59', '60', '61']). % Pairs
card_instances_commons('203', ['54', '55', '56', '57']). % Small groups
card_instances_commons('204', ['62', '63', '64', '65']). % Medium groups
card_instances_commons('205', ['66', '67', '68', '69']). % Large groups
card_instances_commons('206', ['70', '71', '72', '73']). % Plenary

card_instances_commons('301', ['28', '29', '30', '31']).  % Forum
card_instances_commons('302', ['74', '75', '76', '77']).  % Presentation sw.
card_instances_commons('303', ['78', '79', '80', '81']).  % IWB
card_instances_commons('304', ['82', '172', '84', '85']). % Wiki sw.
card_instances_commons('305', ['86', '87', '88', '89']).  % Videoconference
card_instances_commons('306', ['90', '91', '92', '93']). % Selected study material
card_instances_commons('307', ['94', '95', '96', '97']). % Source of materials
card_instances_commons('308', ['98', '99', '100', '101']).   % Text editor
card_instances_commons('309', ['102', '103', '104', '105']). % Projector
card_instances_commons('310', ['106', '107', '108', '109']). % No technology
card_instances_commons('311', ['110', '111', '112', '113']). % Materials and tools for practice

card_instances_commons('1000', ['1000']). % Call for suggestions
card_instances_commons('1001', ['36', '162', '163', '164', '165', '166', '167', '168',
			'169', '170', '171', '173', '174', '175']). % wildcard

/*
  * toCardType/2
  *    given an intance of card, returns the corresponding type
  *    according to the above defined card_instances_commons/2 predicate.
*/
toCardType('', '') :- !.
toCardType(Instance, Type) :-
	card_instances(Type, ListOfInstances),
	member(Instance, ListOfInstances).


/*
  * task/2: type_id, name
  * 100 < id < 200
*/
task('101', 'WRITING A TEXT').
task('102', 'STUDYING').
task('103', 'FINDING MATERIALS').
task('104', 'PREPARING A LIST').
task('105', 'COMMENTING ON SOMEONE ELSE\'S WORK').
task('106', 'PREPARING A PRESENTATION').
task('107', 'CARRYING OUT AN ASSIGNMENT').
task('108', 'GIVING A PRESENTATION').
task('109', 'SOLVING A PROBLEM').
task('110', 'INTERVIEWING AN EXPERT').
task('111', 'ASSUMING ROLES').
task('112', 'PRODUCING AN ARTEFACT').
task('113', 'DEBATING').

isTask('') :- !.
isTask(Card) :-
	controlCard(Card, _),
	!.
isTask(Card) :-
	task(Card, _),
	!.
isTask(_Card) :-
	fail.


/*
  team/2: type_id, name
  200 < id < 300
*/
team('201', 'INDIVIDUAL LEARNERS').
team('202', 'PAIRS').
team('203', 'SMALL GROUPS').
team('204', 'MEDIUM-SIZED GROUPS').
team('205', 'LARGE GROUPS').
team('206', 'PLENARY').

isTeam('') :- !.
isTeam(Card) :-
	controlCard(Card, _),
	!.
isTeam(Card) :-
	team(Card, _),
	!.
isTeam(_Card) :-
	fail.


/*
  technology/2: type_id, name
  300 < id < 400
*/
technology('301', 'FORUM').
technology('302', 'PRESENTATION SOFTWARE').
technology('303', 'INTERACTIVE WHITEBOARD (IWB)').
technology('304', 'WIKI SOFTWARE').
technology('305', 'VIDEOCONFERENCING SYSTEM').
technology('306', 'SELECTED STUDY MATERIALS').
technology('307', 'SOURCE OF MATERIALS FOR LEARNING').
technology('308', 'TEXT EDITOR').
technology('309', 'PROJECTOR').
technology('310', 'NO TECHNOLOGY').
technology('311', 'MATERIALS AND TOOLS FOR PRACTICE').

isTechnology('') :- !.
isTechnology(Card) :-
	controlCard(Card, _),
	!.
isTechnology(Card) :-
	technology(Card, _),
	!.
isTechnology(_Card) :-
	fail.


/*
  control cards/2: type_id, name
*/
controlCard('1000', 'call-for-suggestion').
controlCard('1001', 'wildcard').


wrongCardTypes([column(ColN, Technique, Task1, Team1, Tech1a, Tech1b, Task2, Team2, Tech2a, Tech2b)|_], [SlotID], [error(Code, Description)]) :-
    controlCard(WildcardID, 'wildcard'),
    (
        (Technique == WildcardID,
            format(atom(SlotID), 'C~d-CSW-TECHNIQUE', [ColN]),
            Code = '098',
            error_details(Code, Description)
        );
        (Task1 == WildcardID,
            format(atom(SlotID), 'C~d-ABA-CSS-TASK', [ColN]),
            Code = '098',
            error_details(Code, Description)
        );
        (Team1 == WildcardID,
            format(atom(SlotID), 'C~d-ABA-CSS-TEAM', [ColN]),
            Code = '098',
            error_details(Code, Description)
        );
        (Tech1a == WildcardID,
            format(atom(SlotID), 'C~d-ABA-CSS-TEC1', [ColN]),
            Code = '098',
            error_details(Code, Description)
        );
        (Tech1b == WildcardID,
            format(atom(SlotID), 'C~d-ABA-CSS-TEC2', [ColN]),
            Code = '098',
            error_details(Code, Description)
        );
        (Task2 == WildcardID,
            format(atom(SlotID), 'C~d-ABB-CSS-TASK', [ColN]),
            Code = '098',
            error_details(Code, Description)
        );
        (Team2 == WildcardID,
            format(atom(SlotID), 'C~d-ABB-CSS-TEAM', [ColN]),
            Code = '098',
            error_details(Code, Description)
        );
        (Tech2a == WildcardID,
            format(atom(SlotID), 'C~d-ABB-CSS-TEC1', [ColN]),
            Code = '098',
            error_details(Code, Description)
        );
        (Tech2b == WildcardID,
            format(atom(SlotID), 'C~d-ABB-CSS-TEC2', [ColN]),
            Code = '098',
            error_details(Code, Description)
        );
        (not(isTechnique(Technique)),
            format(atom(SlotID), 'C~d-CSW-TECHNIQUE', [ColN]),
            Code = '099',
            error_details(Code, Description)
        );
        (not(isTask(Task1)),
            format(atom(SlotID), 'C~d-ABA-CSS-TASK', [ColN]),
            Code = '099',
            error_details(Code, Description)
        );
        (not(isTeam(Team1)),
            format(atom(SlotID), 'C~d-ABA-CSS-TEAM', [ColN]),
            Code = '099',
            error_details(Code, Description)
        );
        (not(isTechnology(Tech1a)),
            format(atom(SlotID), 'C~d-ABA-CSS-TEC1', [ColN]),
            Code = '099',
            error_details(Code, Description)
        );
        (not(isTechnology(Tech1b)),
            format(atom(SlotID), 'C~d-ABA-CSS-TEC2', [ColN]),
            Code = '099',
            error_details(Code, Description)
        );
        (not(isTask(Task2)),
            format(atom(SlotID), 'C~d-ABB-CSS-TASK', [ColN]),
            Code = '099',
            error_details(Code, Description)
        );
        (not(isTeam(Team2)),
            format(atom(SlotID), 'C~d-ABB-CSS-TEAM', [ColN]),
            Code = '099',
            error_details(Code, Description)
        );
        (not(isTechnology(Tech2a)),
            format(atom(SlotID), 'C~d-ABB-CSS-TEC1', [ColN]),
            Code = '099',
            error_details(Code, Description)
        );
        (not(isTechnology(Tech2b)),
            format(atom(SlotID), 'C~d-ABB-CSS-TEC2', [ColN]),
            Code = '099',
            error_details(Code, Description)
        )
    ), !.
wrongCardTypes([_|Columns], Errors, Descriptions) :-    
    wrongCardTypes(Columns, Errors, Descriptions), !.



/*
  *
  * taskPrerequisites/2
  *
  * taskPrerequisites(+TaskId, -ListOfTaskIds)
  *
  * TaskId should be preceded by at least one of ListOfTaskIds
  *
  * TASK PREREQUISITES (table 4 in 3T_checks.docx document):
  * COMMENTING ON SOMEONE ELSE'S WORK should follow WRITING A TEXT
  *                                              or PREPARING A LIST
  *                                              or PREPARING A PRESENTATION
  *                                              or CARRYING OUT AN ASSIGNMENT
  *                                              or SOLVING A PROBLEM
  *                                              or PRODUCING AN ARTEFACT
  * GIVING A PRESENTATION             should follow WRITING A TEXT
  *                                              or PREPARING A PRESENTATION
  *                                              or CARRYING OUT AN ASSIGNMENT
  *                                              or SOLVING A PROBLEM
  *                                              or PRODUCING AN ARTEFACT
  * INTERVIEWING AN EXPERT            should follow PREPARING A LIST
  *
  * All other tasks have no prerequisites.
  * A "call for suggestion" card in a task slot always satisfies any prerequisite of
  *  other task cards in subsequent slots.
  */
taskPrerequisites('105', ['101', '104', '106', '107', '109', '112']).
taskPrerequisites('108', ['101', '106', '107', '109', '112']).
taskPrerequisites('110', ['104']).
taskPrerequisites(Task, []) :-
    Task \= '105',
    Task \= '108',
    Task \= '110'.

/*
  * taskSlotPairSequence/2
  * taskSlotPairSequence(+Columns, -TaskSlotPairSequence)
  *
  * TaskSlotPairSequence is an ordered list representing the sequence of {task,slot} pairs specified
  * in the board. The list has a maximum of 8 pairs. Pairs are listed in
  * column order; within a column, 1st row task-slot pair precedes 2nd row one.
  * If a task slot is empty, it is not included in the list.
  */
taskSlotPairSequence([], []).
taskSlotPairSequence([Column | Columns], AllTasks) :-
    taskInColumn(Column, TheseTasks),
    taskSlotPairSequence(Columns, OtherTasks),
    append(TheseTasks, OtherTasks, AllTasks).   % è da bambini, ma ho fretta

taskInColumn(column(_, _, '', _, _, _, '', _, _, _), []) :- !.
taskInColumn(column(ColNumber, _, '', _, _, _, LowerTask, _, _, _),
             [taskInSlot(LowerTask, SlotName)]) :-
    format(string(SlotName), 'C~d-ABB-CSS-TASK', [ColNumber]),
    !.
taskInColumn(column(ColNumber, _, UpperTask, _, _, _, '', _, _, _),
             [taskInSlot(UpperTask, SlotName)]) :-
    format(string(SlotName), 'C~d-ABA-CSS-TASK', [ColNumber]),
    !.
taskInColumn(column(ColNumber, _, UpperTask, _, _, _, LowerTask, _, _, _),
             [taskInSlot(UpperTask, SlotName1), taskInSlot(LowerTask, SlotName2)]) :-
    format(string(SlotName1), 'C~d-ABA-CSS-TASK', [ColNumber]),
    format(string(SlotName2), 'C~d-ABB-CSS-TASK', [ColNumber]).



/*
  *   Board representation:
  *
  *   board(+Level, +CheckCompleteness, -ListOfColumns).

  *   column(ColNumber, Technique,
  *          TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
  *          TaskABB, TeamABB, TechnologyABB1, TechnologyABB2).
  *
*/

/* Slot names:
   unqualified -> same for any column
   qualified -> prefixed with 'C<n>-', with n in {1..4}
*/
unqualifiedSlotNames(['CSW-TECHNIQUE',
		      'ABA-CSS-TASK', 'ABA-CSS-TEAM', 'ABA-CSS-TEC1', 'ABA-CSS-TEC2',
		      'ABB-CSS-TASK', 'ABB-CSS-TEAM', 'ABB-CSS-TEC1', 'ABB-CSS-TEC2']).
qualifiedSlotNames(ColNumber, ListOfNames) :-
	unqualifiedSlotNames(ULNs),
	format(atom(Prefix), 'C~d-', [ColNumber]),
	maplist(atom_concat(Prefix), ULNs, ListOfNames). % curried

convertSlotNameIntoGenericError(SlotName, error('201', GenericError)) :-
    sub_string(SlotName, _, _, 0, '-TECHNIQUE'),
    error_details('201', GenericError),!.
convertSlotNameIntoGenericError(SlotName, error('202', GenericError)) :-
    sub_string(SlotName, _, _, 0, '-TASK'),
    error_details('202', GenericError),!.
convertSlotNameIntoGenericError(SlotName, error('203', GenericError)) :-
    sub_string(SlotName, _, _, 0, '-TEAM'),
    error_details('203', GenericError),!.
convertSlotNameIntoGenericError(SlotName, error('204', GenericError)) :-
    sub_string(SlotName, _, _, 1, '-TEC'),
    error_details('204', GenericError),!.
convertSlotNameIntoGenericError(SlotName, error('200', GenericError)) :-
    error_details('200', GenericError),
    debug_format('Unrecognized slot name: [~w]~n', [SlotName]),!.

/*
  * error_details/2
  *
  * error codes and (default) descriptions for error details
*/
error_details('001', 'This task must be preceded by a prerequiste task (should we list them?)').
error_details('002', 'The combination of task, team and technology(ies) is not recognized as a valid pattern').
error_details('003', 'The combination of task, team and technology(ies) has a one week duration, which makes it incompatible in the position it is used').
error_details('004', 'This technology is not compatible with the specified team').
error_details('005', 'You cannot specify the same technology twice for the same task').
error_details('006', 'The specified technologies are not compatible one with the other').
error_details('007', 'The two tasks in this column have both a one week duration and cannot be placed in the same week').
error_details('098', 'Unexpected wildcard found in this slot').
error_details('099', 'Unexpected card type found in this slot').
error_details('200', 'Invalid card').
error_details('201', 'This Technique card is not placed in the appropriate position. Read the Technique card again to check phase position.').
error_details('202', 'This Task is not the one expected in this slot by this instance of Technique. Read the Technique card again and make a different choice.').
error_details('203', 'This Team is not the one expected in this slot by this instance of Technique. Read the Technique card again and make a different choice.').
error_details('204', 'This Technology is not the one expected in this slot by this instance of Technique. Read the Technique card again and make a different choice.').



/*
  * +++++++++++ U T I L I T I E S +++++++++++
  *
  *
*/

/*
  *  debug_write  &  debug_format
  *     output directed to user_error stream (the interpreter console)
  *
  *  To remove debugging info just turn the debug/0 predicate to true.
*/

debug :- true.

debug_writeln(X) :-
	debug,
	!,
	writeln(user_error, X).
debug_writeln(_X).

debug_format(X, Y) :-
	debug,
	!,
	format(user_error, X, Y).
debug_format(_X, _Y).

is_empty(Atom) :- var(Atom) ; atom_length(Atom, 0).
is_not_empty(Atom) :- nonvar(Atom), atom_length(Atom, L), L @>0.

not_both_empty(T1, T2) :-
	is_empty(T1),
	is_empty(T2),
	!,
	fail.
not_both_empty(_T1, _T2).

is_empty_column(column(_ColNumber, Technique,
		       TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
	is_empty(Technique),
	is_empty(TaskABA),
	is_empty(TeamABA),
	is_empty(TechnologyABA1),
	is_empty(TechnologyABA2),
	is_empty(TaskABB),
	is_empty(TeamABB),
	is_empty(TechnologyABB1),
	is_empty(TechnologyABB2).

non_empty_slot_in_col(column(N, T, _, _, _, _, _, _, _, _), Slot) :-
    is_not_empty(T),
    !,
    format(atom(Slot), 'C~d-CSW-TECHNIQUE', [N]).
non_empty_slot_in_col(column(N, _, T, _, _, _, _, _, _, _), Slot) :-
    is_not_empty(T),
    !,
    format(atom(Slot), 'C~d-ABA-CSS-TASK', [N]).
non_empty_slot_in_col(column(N, _, _, T, _, _, _, _, _, _), Slot) :-
    is_not_empty(T),
    !,
    format(atom(Slot), 'C~d-ABA-CSS-TEAM', [N]).
non_empty_slot_in_col(column(N, _, _, _, T, _, _, _, _, _), Slot) :-
    is_not_empty(T),
    !,
    format(atom(Slot), 'C~d-ABA-CSS-TEC1', [N]).
non_empty_slot_in_col(column(N, _, _, _, _, T, _, _, _, _), Slot) :-
    is_not_empty(T),
    !,
    format(atom(Slot), 'C~d-ABA-CSS-TEC2', [N]).
non_empty_slot_in_col(column(N, _, _, _, _, _, T, _, _, _), Slot) :-
    is_not_empty(T),
    !,
    format(atom(Slot), 'C~d-ABB-CSS-TASK', [N]).
non_empty_slot_in_col(column(N, _, _, _, _, _, _, T, _, _), Slot) :-
    is_not_empty(T),
    !,
    format(atom(Slot), 'C~d-ABB-CSS-TEAM', [N]).
non_empty_slot_in_col(column(N, _, _, _, _, _, _, _, T, _), Slot) :-
    is_not_empty(T),
    !,
    format(atom(Slot), 'C~d-ABB-CSS-TEC1', [N]).
non_empty_slot_in_col(column(N, _, _, _, _, _, _, _, _, T), Slot) :-
    is_not_empty(T),
    !,
    format(atom(Slot), 'C~d-ABB-CSS-TEC2', [N]).
non_empty_slot_in_col(_Col, _Slot) :-
    fail.

append_dl(Xs-Ys, Ys-Zs, Xs-Zs).

/* Removes a single suggestion card from board, if present */
removeSuggestionCard([], []).
removeSuggestionCard([Col | Cols], [Col1 | Cols1]) :-
    removeSCinCol(Col, Col1),
    removeSuggestionCard(Cols, Cols1).

removeSCinCol(column(N, '1000',
                                TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                                TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                            column(N, '',
                                TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                                TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    !.
removeSCinCol(column(N, Technology,
                                '1000', TeamABA, TechnologyABA1, TechnologyABA2,
                                TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                            column(N, Technology,
                                '', TeamABA, TechnologyABA1, TechnologyABA2,
                                TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    !.
removeSCinCol(column(N, Technology,
                                TaskABA, '1000', TechnologyABA1, TechnologyABA2,
                                TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                            column(N, Technology,
                                TaskABA, '', TechnologyABA1, TechnologyABA2,
                                TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    !.
removeSCinCol(column(N, Technology,
                                TaskABA, TeamABA, '1000', TechnologyABA2,
                                TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                            column(N, Technology,
                                TaskABA, TeamABA, '', TechnologyABA2,
                                TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    !.
removeSCinCol(column(N, Technology,
                                TaskABA, TeamABA, TechnologyABA1, '1000',
                                TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                            column(N, Technology,
                                TaskABA, TeamABA, TechnologyABA1, '',
                                TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    !.
removeSCinCol(column(N, Technology,
                                TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                                '1000', TeamABB, TechnologyABB1, TechnologyABB2),
                            column(N, Technology,
                                TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                                '', TeamABB, TechnologyABB1, TechnologyABB2)) :-
    !.
removeSCinCol(column(N, Technology,
                                TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                                TaskABB, '1000', TechnologyABB1, TechnologyABB2),
                            column(N, Technology,
                                TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                                TaskABB, '', TechnologyABB1, TechnologyABB2)) :-
    !.
removeSCinCol(column(N, Technology,
                                TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                                TaskABB, TeamABB, '1000', TechnologyABB2),
                            column(N, Technology,
                                TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                                TaskABB, TeamABB, '', TechnologyABB2)) :-
    !.
removeSCinCol(column(N, Technology,
                                TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                                TaskABB, TeamABB, TechnologyABB1, '1000'),
                            column(N, Technology,
                                TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                                TaskABB, TeamABB, TechnologyABB1, '')) :-
    !.
removeSCinCol(Col, Col).

/*
  unbindSlotsWithSuggestion(SuggestionVar, SuggestionType, Cols, Cols1)
  All occurrences of SuggestionCard '1000' in Cols are substituted by SuggestionVar in Cols1.
  All occurrences of '' in Cols are substituted by _ (unnamed var) in Cols1
  All other nonvar elements in Cols are unchanged in Cols1
  */
unbindSlotsWithSuggestion(_, _, [], []).
unbindSlotsWithSuggestion(SuggestionVar, SuggestionType, [Col | Cols], [ModCol | ModCols]) :-
    Col =.. LCol,
    unbindColumnWithSuggestion(SuggestionVar, SuggestionType, LCol, ULCol),
    ModCol =.. ULCol,
    unbindSlotsWithSuggestion(SuggestionVar, SuggestionType, Cols, ModCols).

unbindColumnWithSuggestion(_, _, [], []).
unbindColumnWithSuggestion(SuggestionVar, SuggestionType, [SuggestionCard | Rest], [SuggestionVar | Rest1]) :-
  controlCard(SuggestionCard, 'call-for-suggestion'),
  length(Rest, Pos),
  (Pos == 0,  %tech
    SuggestionType = technology;
   Pos == 1,  %tech
    SuggestionType = technology;
   Pos == 2,  %team
    SuggestionType = team;
   Pos == 3,  %task
    SuggestionType = task;
   Pos == 4,  %tech
    SuggestionType = technology;
   Pos == 5,  %tech
    SuggestionType = technology;
   Pos == 6,  %team
    SuggestionType = team;
   SuggestionType = task
  ),
  unbindColumnWithSuggestion(SuggestionVar, SuggestionType, Rest, Rest1).
unbindColumnWithSuggestion(SuggestionVar, SuggestionType, [Card | Rest], [_ | Rest1]) :-
    is_empty(Card),
    unbindColumnWithSuggestion(SuggestionVar, SuggestionType, Rest, Rest1).
unbindColumnWithSuggestion(SuggestionVar, SuggestionType, [Val | Rest], [Val | Rest1]) :-
    unbindColumnWithSuggestion(SuggestionVar, SuggestionType, Rest, Rest1).



