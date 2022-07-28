% -*- Mode: Prolog -*-

:- module(suggestionsBase, [provideSuggestions_base/2]).

:- use_module(kbCommons).
:- use_module(kbBase).


/*
  SUPERSEDED: >>> If all technique cards are empty, no suggestions are provided <<<
  THIS REQUIREMENT HAS CHANGED in october 2020: suggestions are now offered even
   in absence of technique cards.
  This behaviour is implemented in the provideSuggestions3T/2 predicate.
  */
provideSuggestions_base(Columns, CardIds) :-
    /* if no technique card is present, and there is a suggestion card
     * somewhere in the 3T slots
     */
    Columns = [ column(1, '', TaskABA1, TeamABA1, SuggestionCard1, TechnologyABA21,
                              TaskABB1, TeamABB1, TechnologyABB11, TechnologyABB21),
                column(2, '', TaskABA2, TeamABA2, SuggestionCard2, TechnologyABA22,
                              TaskABB2, TeamABB2, TechnologyABB12, TechnologyABB22),
                column(3, '', TaskABA3, TeamABA3, SuggestionCard3, TechnologyABA23,
                              TaskABB3, TeamABB3, TechnologyABB13, TechnologyABB23),
	            column(4, '', TaskABA4, TeamABA4, SuggestionCard4, TechnologyABA24,
                              TaskABB4, TeamABB4, TechnologyABB14, TechnologyABB24) ],
    controlCard(SuggestionCard, 'call-for-suggestion'),
    member(SuggestionCard, [TaskABA1, TeamABA1, SuggestionCard1, TechnologyABA21,
                            TaskABB1, TeamABB1, TechnologyABB11, TechnologyABB21,
                            TaskABA2, TeamABA2, SuggestionCard2, TechnologyABA22,
                            TaskABB2, TeamABB2, TechnologyABB12, TechnologyABB22,
                            TaskABA3, TeamABA3, SuggestionCard3, TechnologyABA23,
                            TaskABB3, TeamABB3, TechnologyABB13, TechnologyABB23,
                            TaskABA4, TeamABA4, SuggestionCard4, TechnologyABA24,
                            TaskABB4, TeamABB4, TechnologyABB14, TechnologyABB24]),
    !,
    provideSuggestions3T(Columns, Suggestions),
    mapToCardInstances(Suggestions, CardIds).
/*
  At least one Technique is non empty
  */
provideSuggestions_base(Cols, CardIds) :-
	unbindSlotsWithSuggestion(SuggestionVar, _, Cols, Term),
	Pattern =.. [boardPatternExpanded, Term],
	% debug_format('provide suggestions - pattern:~n~p~n', [Pattern]),
	findall(SuggestionVar, Pattern, Bag),
	sort(Bag, Set),
	% debug_format('-- Set: ~w~n', [Set]),
	removeEmptyAtoms(Set, UniqueSuggestions),
	mapToCardInstances(UniqueSuggestions, CardIds).

mapToCardInstances([], []).
mapToCardInstances([CardType | CardTypes], [InstanceId | InstanceIds]) :-
	card_instances(CardType, [InstanceId | _]), % always takes the first instance - BEWARE
	mapToCardInstances(CardTypes, InstanceIds).
mapToCardInstances([InstanceId | CardTypes], [InstanceId | InstanceIds]) :- % if the previous one fails, maybe this got called with (some?) card instances
  debug_format('error converting card type [~p] to card instance; using ~p as card instance~n', [InstanceId, InstanceId]),
  mapToCardInstances(CardTypes, InstanceIds).

/*
  removeUnbounds(+L1, -L2)
  L2 contains the nonvar elements of L1. All var elements are removed.
  */
removeUnbounds([], []).
removeUnbounds([H | T], T1) :-
	var(H),
	removeUnbounds(T, T1).
removeUnbounds([H | T], [H |T1]) :-
	nonvar(H),
	removeUnbounds(T, T1).

/*
  removeEmptyAtoms(+L1, -L2)
  L2 contains the elements of L1 that are nonvar and different prom ''.
  All var elements are removed.
  All '' elements are removed.
  */
removeEmptyAtoms([], []).
removeEmptyAtoms([H | T], T1) :-
	var(H),
	removeEmptyAtoms(T, T1).
removeEmptyAtoms(['' | T], T1) :-
	removeEmptyAtoms(T, T1).
removeEmptyAtoms([H | T], [H |T1]) :-
	nonvar(H),
	H \= '',
	removeEmptyAtoms(T, T1).


/*
  * provideSuggestions3T/2
  *
  * provideSuggestions3T(+ListOfColumns, -CardIds)
  *
  * If the suggestion card is placed in a task slot, see if it can be a prerequisite
  *  of following tasks; the suggestion should not be already present in previous task
  *  slots, and it should comply with time constraints.
  *
  * After having checked prerequisites, if the suggestion card is placed in a task slot,
  * and at least a team or technology card is present in the same column and row,
  * suggestions are based on Table 1 in 3T_checks.docx
  *
  * If the suggestion card is placed on a team or technology slot, and a task card is
  *  present in the same column and row, suggestions are based on Table 1 in 3T_checks.docx
  *
  * If the suggestion card is placed on a team or technology slot, and a task card is not
  *  present in the same column and row, suggestions are based on Table 2 in 3T_checks.docx
  *
  * 2nd and 3rd cases ar handled by the provideSuggestions2T/3 predicate
  */
provideSuggestions3T(Columns, AllSuggestions) :-
    %debug_format('searching for 3T suggestions in 4T scenario~n', []),
    controlCard(SuggestionCard, 'call-for-suggestion'),
    taskSlotPairSequence(Columns, TaskSlotPairSequence),
    member(taskInSlot(SuggestionCard, _), TaskSlotPairSequence),
    prerequisiteSuggestion(TaskSlotPairSequence, Suggestions),
    subtract(Suggestions, [SuggestionCard], PrerequisiteSuggestions),
    !,
    suggestTaskBasedOn2T(SuggestionCard, Columns, SuggestionsBasedOn2T),
    union(PrerequisiteSuggestions, SuggestionsBasedOn2T, AllSuggestions).
provideSuggestions3T(Columns, CardIds) :-
    controlCard(SuggestionCard, 'call-for-suggestion'),
    provideSuggestions2T(SuggestionCard, Columns, CardIds).

/*
  * prerequisiteSuggestion/2
  * prerequisiteSuggestion(+TaskSlotPairSequence, -UnsatisfiedRequirements)
  *
  * prerequisiteSuggestion/3
  * prerequisiteSuggestion(+PrecedingTasks, +FollowingTasks, -CardIds)
  *
  * Does any task following the suggestion card need a prerequisite that is not
  *  yet present in the preceding tasks?
  *  If so, return the needed prerequisite(s)
  *
  * Does any task preceding the suggestion card act as a prerequisite for any task
  *  that is not already present amongst the folloging tasks?
  *  If so, return the requiring task(s).
  *
  * prerequisiteSuggestion/2 never fails: if prerequisiteSuggestion/3 cannot
  *  idenitify any prerequisite-based suggestions, prerequisiteSuggestion/2
  *  reurns an empty UnsatisfiedRequirements list.
  *
  */
prerequisiteSuggestion(TaskSlotPairSequence, UnsatisfiedRequirements) :-
    extractPrecedingAndFollowingTasks(TaskSlotPairSequence, PrecedingTasks, FollowingTasks),
    % debug_format('  preceding tasks: ~w~n', [PrecedingTasks]),
    % debug_format('  following tasks: ~w~n', [FollowingTasks]),
    prerequisiteSuggestion(PrecedingTasks, FollowingTasks, UnsatisfiedRequirements),
    !.
prerequisiteSuggestion(_TaskSlotPairSequence, []).

prerequisiteSuggestion(PrecedingTasks, FollowingTasks, UnsatisfiedRequirements) :-
/*  Does any task following the suggestion card need a prerequisite that is not
  *  yet present in the preceding tasks?
  *  If so, return the needed prerequisite(s)
  */
    findall(Requirements,
            (member(Task, FollowingTasks), taskPrerequisites(Task, Requirements)),
            NestedRequirements),
    flatten(NestedRequirements, UnsortedRequirements),
    sort(UnsortedRequirements, AllRequirements),
    subtract(AllRequirements, PrecedingTasks, UnsatisfiedRequirements),
    % debug_format('  Unsatisfied Requirements: ~w~n', [UnsatisfiedRequirements]),
    UnsatisfiedRequirements \= [],
    !.
prerequisiteSuggestion(PrecedingTasks, FollowingTasks, UnexploitedPrerequisites) :-
/*  Does any task preceding the suggestion card act as a prerequisite for any task
  *  that is not already present amongst the folloging tasks?
  *  If so, return the requiring task(s).
  */
    % debug_format('  prerequisiteSuggestion enters 2nd clause~n', []),
    findall(UnexploitedPrerequisite,
            (member(Task, PrecedingTasks),
             taskPrerequisites(UnexploitedPrerequisite, Prerequisites),
             member(Task, Prerequisites)),
            NestedPrerequisites),
    % debug_format('  Nested Prerequisites: ~w~n', [NestedPrerequisites]),
    flatten(NestedPrerequisites, UnsortedPrerequisites),
    sort(UnsortedPrerequisites, AllPreRequisites),
    subtract(AllPreRequisites, FollowingTasks, UnexploitedPrerequisites),
    % debug_format('  Unexploited Prerequisites: ~w~n', [UnexploitedPrerequisites]).
    UnexploitedPrerequisites \= [],
    !.
/* prerequisiteSuggestion/3 fails if no prerequisite-based suggestions can be identified */


extractPrecedingAndFollowingTasks([], [], []).
extractPrecedingAndFollowingTasks([taskInSlot(SuggestionCard, _Slot) | OtherPairs],
                                  [], FollowingTasks) :-
    controlCard(SuggestionCard, 'call-for-suggestion'),
    !,          % stop scannning for preceding,start scanning for following tasks
    extractFollowingTasks(OtherPairs, FollowingTasks).
extractPrecedingAndFollowingTasks([taskInSlot(TaskCard, _Slot) | OtherPairs],
                                  [TaskCard | OtherTaskCards], FollowingTasks) :-
                % still scanning for preceding tasks
    extractPrecedingAndFollowingTasks(OtherPairs, OtherTaskCards, FollowingTasks).

extractFollowingTasks([], []).
extractFollowingTasks([taskInSlot(TaskCard, _Slot) | OtherPairs],
                      [TaskCard | OtherTaskCards]) :-
    extractFollowingTasks(OtherPairs, OtherTaskCards).


/*
  * Tabella 1: Task → [Teams], [Technologies], {week | synchronous}
  *   (https://drive.google.com/file/d/1Xta7jpMoQ4egHuSb0_cBx2WTm0absgFG/view?usp=sharing)
  *   Team list represents a disjunction of elements,
  *   Technology list represents a conjunction of elements
  *
  */
table_1('101', ['201', '202', '203'], ['308', ''], week). % WRITING A TEXT
table_1('101', ['201', '202', '203'], ['', '308'], week). % WRITING A TEXT
table_1('101', ['201', '202', '203'], ['304', ''], week). % WRITING A TEXT
table_1('101', ['201', '202', '203'], ['', '304'], week). % WRITING A TEXT
table_1('101', ['201', '202', '203'], ['308', '301'], week). % WRITING A TEXT
table_1('101', ['201', '202', '203'], ['301', '308'], week). % WRITING A TEXT
table_1('101', ['201', '202', '203'], ['304', '301'], week). % WRITING A TEXT
table_1('101', ['201', '202', '203'], ['301', '304'], week). % WRITING A TEXT
table_1('101', ['201', '202', '203'], ['308', '310'], week). % WRITING A TEXT
table_1('101', ['201', '202', '203'], ['310', '308'], week). % WRITING A TEXT
table_1('101', ['201', '202', '203'], ['304', '310'], week). % WRITING A TEXT
table_1('101', ['201', '202', '203'], ['310', '304'], week). % WRITING A TEXT

table_1('102', ['201', '202', '203'], ['306'], week). % STUDYING

table_1('103', ['201', '202', '203'], ['307'], week). % FINDING MATERIALS

table_1('104', ['201', '202', '203', '204', '205', '206'], ['301', ''], week). % PREPARING A LIST
table_1('104', ['201', '202', '203', '204', '205', '206'], ['', '301'], week). % PREPARING A LIST
table_1('104', ['201', '202', '203', '204', '205', '206'], ['308', ''], week). % PREPARING A LIST
table_1('104', ['201', '202', '203', '204', '205', '206'], ['', '308'], week). % PREPARING A LIST
table_1('104', ['201', '202', '203', '204', '205', '206'], ['301', '308'], week). % PREPARING A LIST
table_1('104', ['201', '202', '203', '204', '205', '206'], ['308', '301'], week). % PREPARING A LIST
table_1('104', ['201', '202', '203', '204', '205', '206'], ['308', '305'], synchronous). % PREPARING A LIST
table_1('104', ['201', '202', '203', '204', '205', '206'], ['305', '308'], synchronous). % PREPARING A LIST
table_1('104', ['201', '202', '203', '204', '205', '206'], ['308', '310'], synchronous). % PREPARING A LIST
table_1('104', ['201', '202', '203', '204', '205', '206'], ['310', '308'], synchronous). % PREPARING A LIST

table_1('105', ['201', '202', '203'], ['308', ''], week). % COMMENTING ON SOMEONE ELSE\'S WORK
table_1('105', ['201', '202', '203'], ['', '308'], week). % COMMENTING ON SOMEONE ELSE\'S WORK
table_1('105', ['201', '202', '203'], ['304', ''], week). % COMMENTING ON SOMEONE ELSE\'S WORK
table_1('105', ['201', '202', '203'], ['', '304'], week). % COMMENTING ON SOMEONE ELSE\'S WORK
table_1('105', ['201', '202', '203'], ['308', '301'], week). % COMMENTING ON SOMEONE ELSE\'S WORK
table_1('105', ['201', '202', '203'], ['301', '308'], week). % COMMENTING ON SOMEONE ELSE\'S WORK
table_1('105', ['201', '202', '203'], ['304', '301'], week). % COMMENTING ON SOMEONE ELSE\'S WORK
table_1('105', ['201', '202', '203'], ['301', '304'], week). % COMMENTING ON SOMEONE ELSE\'S WORK
table_1('105', ['201', '202', '203'], ['308', '310'], week). % COMMENTING ON SOMEONE ELSE\'S WORK
table_1('105', ['201', '202', '203'], ['310', '308'], week). % COMMENTING ON SOMEONE ELSE\'S WORK
table_1('105', ['201', '202', '203'], ['304', '310'], week). % COMMENTING ON SOMEONE ELSE\'S WORK
table_1('105', ['201', '202', '203'], ['310', '304'], week). % COMMENTING ON SOMEONE ELSE\'S WORK

table_1('106', ['201', '202', '203', '204'], ['302', '301'], week). % PREPARING A PRESENTATION
table_1('106', ['201', '202', '203', '204'], ['302', '303'], week). % PREPARING A PRESENTATION
table_1('106', ['201', '202', '203', '204'], ['302', '310'], week). % PREPARING A PRESENTATION
table_1('106', ['201', '202', '203', '204'], ['301', '302'], week). % PREPARING A PRESENTATION
table_1('106', ['201', '202', '203', '204'], ['303', '302'], week). % PREPARING A PRESENTATION
table_1('106', ['201', '202', '203', '204'], ['310', '302'], week). % PREPARING A PRESENTATION

table_1('107', ['201', '202', '203'], ['311', '301'], week). % CARRYING OUT AN ASSIGNMENT
table_1('107', ['201', '202', '203'], ['311', '303'], week). % CARRYING OUT AN ASSIGNMENT
table_1('107', ['201', '202', '203'], ['311', '310'], week). % CARRYING OUT AN ASSIGNMENT
table_1('107', ['201', '202', '203'], ['301', '311'], week). % CARRYING OUT AN ASSIGNMENT
table_1('107', ['201', '202', '203'], ['313', '311'], week). % CARRYING OUT AN ASSIGNMENT
table_1('107', ['201', '202', '203'], ['310', '311'], week). % CARRYING OUT AN ASSIGNMENT

table_1('108', ['206'], ['309', ''], synchronous). % GIVING A PRESENTATION
table_1('108', ['206'], ['303', ''], synchronous). % GIVING A PRESENTATION
table_1('108', ['206'], ['305', ''], synchronous). % GIVING A PRESENTATION
table_1('108', ['206'], ['', '309'], synchronous). % GIVING A PRESENTATION
table_1('108', ['206'], ['', '303'], synchronous). % GIVING A PRESENTATION
table_1('108', ['206'], ['', '305'], synchronous). % GIVING A PRESENTATION

table_1('109', ['201', '202', '203', '206'], ['308', '310'], synchronous). % SOLVING A PROBLEM
table_1('109', ['201', '202', '203', '206'], ['310', '308'], synchronous). % SOLVING A PROBLEM
table_1('109', ['201', '202', '203', '206'], ['308', '305'], synchronous). % SOLVING A PROBLEM
table_1('109', ['201', '202', '203', '206'], ['305', '308'], synchronous). % SOLVING A PROBLEM
table_1('109', ['201', '202', '203', '206'], ['301', ''], week). % SOLVING A PROBLEM
table_1('109', ['201', '202', '203', '206'], ['', '301'], week). % SOLVING A PROBLEM
table_1('109', ['201', '202', '203', '206'], ['308', ''], week). % SOLVING A PROBLEM
table_1('109', ['201', '202', '203', '206'], ['', '308'], week). % SOLVING A PROBLEM
table_1('109', ['201', '202', '203', '206'], ['301', '308'], week). % SOLVING A PROBLEM
table_1('109', ['201', '202', '203', '206'], ['308', '301'], week). % SOLVING A PROBLEM

table_1('110', ['204', '205', '206'], ['310', ''], synchronous). % INTERVIEWING AN EXPERT
table_1('110', ['204', '205', '206'], ['305', ''], synchronous). % INTERVIEWING AN EXPERT
table_1('110', ['204', '205', '206'], ['301', ''], week). % INTERVIEWING AN EXPERT
table_1('110', ['204', '205', '206'], ['', '310'], synchronous). % INTERVIEWING AN EXPERT
table_1('110', ['204', '205', '206'], ['', '305'], synchronous). % INTERVIEWING AN EXPERT
table_1('110', ['204', '205', '206'], ['', '301'], week). % INTERVIEWING AN EXPERT

table_1('111', ['203'], ['310', ''], synchronous). % ASSUMING ROLES
table_1('111', ['203'], ['305', ''], synchronous). % ASSUMING ROLES
table_1('111', ['203'], ['301', ''], week). % ASSUMING ROLES
table_1('111', ['203'], ['', '310'], synchronous). % ASSUMING ROLES
table_1('111', ['203'], ['', '305'], synchronous). % ASSUMING ROLES
table_1('111', ['203'], ['', '301'], week). % ASSUMING ROLES

table_1('112', ['201', '202', '203', '204', '205', '206'], ['311', '303'], week). % PRODUCING AN ARTEFACT
table_1('112', ['201', '202', '203', '204', '205', '206'], ['311', '301'], week). % PRODUCING AN ARTEFACT
table_1('112', ['201', '202', '203', '204', '205', '206'], ['311', '305'], week). % PRODUCING AN ARTEFACT
table_1('112', ['201', '202', '203', '204', '205', '206'], ['303', '311'], week). % PRODUCING AN ARTEFACT
table_1('112', ['201', '202', '203', '204', '205', '206'], ['301', '311'], week). % PRODUCING AN ARTEFACT
table_1('112', ['201', '202', '203', '204', '205', '206'], ['305', '311'], week). % PRODUCING AN ARTEFACT

table_1('113', ['202', '203', '204', '206'], ['310', ''], synchronous). % DEBATING
table_1('113', ['202', '203', '204', '206'], ['305', ''], synchronous). % DEBATING
table_1('113', ['202', '203', '204', '206'], ['301', ''], week). % DEBATING
table_1('113', ['202', '203', '204', '206'], ['', '310'], synchronous). % DEBATING
table_1('113', ['202', '203', '204', '206'], ['', '305'], synchronous). % DEBATING
table_1('113', ['202', '203', '204', '206'], ['', '301'], week). % DEBATING

/*
 * task_vs_technologies_table_1(+Task, -CollectedTechnologies)
 *
 * for a given Task, CollectedTechnologies is the list of all technologies specified in table_1
 * regardless the Time value
 *
 */
task_vs_technologies_table_1(Task, CollectedTechnologies) :-
    setof(Tech, Teams^Techs^Time^(table_1(Task, Teams, Techs, Time),
                                  member(Tech, Techs)),
          CollectedTechnologies).


/*
 * task_vs_technologies_table_1(+Task, +TimeValues, -CollectedTechnologies)
 *
 * for a given Task, CollectedTechnologies is the list of all technologies specified in table_1
 * where the time constraint is a member of the TimeValues list
 *
 */
task_vs_technologies_table_1(Task, TimeValues, CollectedTechnologies) :-
    setof(Tech, Teams^Techs^Time^(table_1(Task, Teams, Techs, Time),
                                  member(Time, TimeValues),
                                  member(Tech, Techs)),
          CollectedTechnologies).

/*
 * task_vs_teams_table_1(+Task, -CollectedTeams)
 *
 * for a given Task, CollectedTeams is the list of all teams specified in table_1
 * regardless the Time value
 *
 */
task_vs_teams_table_1(Task, CollectedTeams) :-
    setof(Team, Teams^Techs^Time^(table_1(Task, Teams, Techs, Time),
                                  member(Team, Teams)),
          CollectedTeams).

/*
 * task_vs_teams_table_1(+Task, +TimeValues, -CollectedTeams)
 *
 * for a given Task, CollectedTeams is the list of all teams specified in table_1
 * where the time constraint is a member of the TimeValues list
 *
 */
task_vs_teams_table_1(Task, TimeValues, CollectedTeams) :-
    setof(Team, Teams^Techs^Time^(table_1(Task, Teams, Techs, Time),
                                  member(Time, TimeValues),
                                  member(Team, Teams)),
          CollectedTeams).


/*
 * team_vs_tasks_table_1(+Team, -CollectedTasks)
 *
 * for a given Team, CollectedTasks is the list of all tasks specified in table_1
 *
 */
team_vs_tasks_table_1(Team, CollectedTasks) :-
    findall(Task, (table_1(Task, Teams, _Techs, _Time),
                   member(Team, Teams)),
            Bag),
    sort(Bag, CollectedTasks).


/*
 * oneTechnology_vs_tasks_table_1(+Technology, -CollectedTasks)
 *
 * for a given Technology, CollectedTasks is the list of all tasks specified in table_1
 *
 */
oneTechnology_vs_tasks_table_1(Technology, CollectedTasks) :-
    findall(Task, (table_1(Task, _Teams, Techs, _Time),
                   member(Technology, Techs)),
            Bag),
    sort(Bag, CollectedTasks).


/*
 * twoTechnologies_vs_tasks_table_1(+Technology1, +Technology2, -CollectedTasks)
 *
 * for two given Technologies, CollectedTasks is the list of all tasks specified in table_1
 *
 */
twoTechnologies_vs_tasks_table_1(Technology1, Technology2, CollectedTasks) :-
    findall(Task, (table_1(Task, _Teams, Techs, _Time),
                   member(Technology1, Techs),
                   member(Technology2, Techs)),
            Bag),
    sort(Bag, CollectedTasks).


/*
 * team_and_one_technology_vs_tasks_table_1(+Team, +Technology, -CollectedTasks)
 *
 * for a given Team and one given Technology, CollectedTasks is the list of all tasks specified in table_1
 *
 */
team_and_one_technology_vs_tasks_table_1(Team, Technology, CollectedTasks) :-
    findall(Task, (table_1(Task, Teams, Techs, _Time),
                   member(Team, Teams),
                   member(Technology, Techs)),
            Bag),
    sort(Bag, CollectedTasks).

/*
 * team_and_two_technologies_vs_tasks_table_1(+Team, +Technology1, +Technology2, -CollectedTasks)
 *
 * for a given Team and two given Technologies, CollectedTasks is the list of all tasks specified in table_1
 *
 */
team_and_two_technologies_vs_tasks_table_1(Team, Technology1, Technology2, CollectedTasks) :-
    findall(Task, (table_1(Task, Teams, Techs, _Time),
                   member(Team, Teams),
                   member(Technology1, Techs),
                   member(Technology2, Techs)),
            Bag),
    sort(Bag, CollectedTasks).



/*
  * Tabella 2: Team → [Technologies]
  *   (https://drive.google.com/file/d/1Xta7jpMoQ4egHuSb0_cBx2WTm0absgFG/view?usp=sharing)
  *   Technology list represents a disjunction of elements
  *
  */
table_2('201', ['302', '304', '306', '308', '310', '311']). % INDIVIDUAL LEARNERS
table_2('202', ['301', '302', '303', '304', '305', '306', '307', '308', '309', '310', '311']). % PAIRS
table_2('203', ['301', '302', '303', '304', '305', '306', '307', '308', '309', '310', '311']). % SMALL GROUPS
table_2('204', ['301', '302', '303', '304', '305', '306', '307', '308', '309', '310', '311']). % MEDIUM-SIZED GROUPS
table_2('205', ['302', '303', '304', '305', '306', '307', '308', '309', '310', '311']). % LARGE GROUPS
table_2('206', ['301', '302', '303', '304', '305', '309', '310']). % PLENARY

team_vs_2techs(Tech1, Tech2, Team) :- % might backtrack
    table_2(Team, Techs),
    member(Tech1, Techs),
    member(Tech2, Techs).

team_vs_1tech(Tech, Team) :- % might backtrack
    table_2(Team, Techs),
    member(Tech, Techs).


/*
  * unbindT2/2
  *
  * unbindT2(+ListOfCardIds, -ListWithoutEmpties)
  *
  * ListWithoutEmpties contains the same elements of ListOfCardIds where
  *   empty elements ('') have been replaced by unbound variables (_)
  *
  */
unbindT2([], []).
unbindT2([T | Ts], [_ | Us]) :-
    is_empty(T),
    !,
    unbindT2(Ts, Us).
unbindT2([T | Ts], [T | Us]) :-
    unbindT2(Ts, Us).

/*
 * suggestTaskBasedOn2T/3
 *
 * suggestTaskBasedOn2T(+SuggestionCard, +Columns, -SuggestionsBasedOn2T)
 *
 * This is called when the suggestion card is in a task slot.
 * SuggestionsBasedOn2T lists all the task cards that are complatible with the
 *  team and/or technology cards in the same row and column as of the suggestion card.
 * Compatibility in based on Table 1 in 3T_checks.docx .
 *
 */
suggestTaskBasedOn2T(_SuggestionCard, [], []).
suggestTaskBasedOn2T(SuggestionCard,
                     [column(_ColNumber, _Technique,
                             SuggestionCard, TeamABA, TechnologyABA1, TechnologyABA2,
                             _TaskABB, _TeamABB, _TechnologyABB1, _TechnologyABB2) | _Columns],
                     SuggestedTasks) :-
    !,
    suggestTask(TeamABA, TechnologyABA1, TechnologyABA2, SuggestedTasks).
suggestTaskBasedOn2T(SuggestionCard,
                     [column(_ColNumber, _Technique,
                             _TaskABA, _TeamABA, _TechnologyABA1, _TechnologyABA2,
                             SuggestionCard, TeamABB, TechnologyABB1, TechnologyABB2) | _Columns],
                     SuggestedTasks) :-
    !,
    suggestTask(TeamABB, TechnologyABB1, TechnologyABB2, SuggestedTasks).
suggestTaskBasedOn2T(SuggestionCard, [_Column | Columns], Suggestions) :-
    suggestTaskBasedOn2T(SuggestionCard, Columns, Suggestions).

/*
 * suggestTask/4
 *
 * suggestTask(+Team, +Technology1, +Technology2, -SuggestedTasks)
 *
 * SuggestedTasks are determinedbt the cards already present in Team, Technology1,
 *  Technology2 slots, based on Table 1 in 3T_checks.docx .
 *
 */
suggestTask(Team, Technology1, Technology2, []) :-
    % if no 2T cards are present, no suggesion is possible.
    is_empty(Team),
    is_empty(Technology1),
    is_empty(Technology2),
    !.
suggestTask(Team, Technology1, Technology2, CollectedTasks) :-
    % debug_format('suggestTask: Team=~w, Tech1=~w, Tech2=~w~n', [Team, Technology1, Technology2]),
    % only a team card
    is_not_empty(Team),
    is_empty(Technology1),
    is_empty(Technology2),
    !,
    team_vs_tasks_table_1(Team, CollectedTasks).
suggestTask(Team, Technology1, Technology2, CollectedTasks) :-
    % debug_format('suggestTask: Team=~w, Tech1=~w, Tech2=~w~n', [Team, Technology1, Technology2]),
    % only a technology card
    is_empty(Team),
    is_not_empty(Technology1),
    is_empty(Technology2),
    !,
    oneTechnology_vs_tasks_table_1(Technology1, CollectedTasks).
suggestTask(Team, Technology1, Technology2, CollectedTasks) :-
    % debug_format('suggestTask: Team=~w, Tech1=~w, Tech2=~w~n', [Team, Technology1, Technology2]),
    % only a technology card
    is_empty(Team),
    is_empty(Technology1),
    is_not_empty(Technology2),
    !,
    oneTechnology_vs_tasks_table_1(Technology2, CollectedTasks).
suggestTask(Team, Technology1, Technology2, CollectedTasks) :-
    % debug_format('suggestTask: Team=~w, Tech1=~w, Tech2=~w~n', [Team, Technology1, Technology2]),
    % two technology cards
    is_empty(Team),
    is_not_empty(Technology1),
    is_not_empty(Technology2),
    !,
    twoTechnologies_vs_tasks_table_1(Technology1, Technology2, CollectedTasks).
suggestTask(Team, Technology1, Technology2, CollectedTasks) :-
    % debug_format('suggestTask: Team=~w, Tech1=~w, Tech2=~w~n', [Team, Technology1, Technology2]),
    % a team card and one technology card
    is_not_empty(Team),
    is_not_empty(Technology1),
    is_empty(Technology2),
    !,
    team_and_one_technology_vs_tasks_table_1(Team, Technology1, CollectedTasks).
suggestTask(Team, Technology1, Technology2, CollectedTasks) :-
    % debug_format('suggestTask: Team=~w, Tech1=~w, Tech2=~w~n', [Team, Technology1, Technology2]),
    % a team card and one technology card
    is_not_empty(Team),
    is_empty(Technology1),
    is_not_empty(Technology2),
    !,
    team_and_one_technology_vs_tasks_table_1(Team, Technology2, CollectedTasks).
suggestTask(Team, Technology1, Technology2, CollectedTasks) :-
    % debug_format('suggestTask: Team=~w, Tech1=~w, Tech2=~w~n', [Team, Technology1, Technology2]),
    % a team card and tw technology cards
    is_not_empty(Team),
    is_not_empty(Technology1),
    is_not_empty(Technology2),
    !,
    team_and_two_technologies_vs_tasks_table_1(Team, Technology1, Technology2, CollectedTasks).
suggestTask(Team, Technology1, Technology2, []) :-
    debug_format('suggestTask catchall - should never happen. ~w ~w ~w~n', [Team, Technology1, Technology2]).

/*
  * provideSuggestions2T/3
  *
  * provideSuggestions2T(+SuggestionCard, +ListOfColumns, -CardIds)
  *
  * Case 2:
  * If the suggestion card is placed on a team or technology slot, and a task card is
  *  present in the same column and row, suggestions are based on Table 1 in 3T_checks.docx
  *  This is carried out by the suggest2T/7 predicate
  *
  * Case 3:
  * If the suggestion card is placed on a team or technology slot, and a task card is not
  *  present in the same column and row, suggestions are based on Table 2 in 3T_checks.docx
  *  This is carried out by the suggest2T/5 predicate
  *
  */
provideSuggestions2T(_SuggestionCard, [], []).
provideSuggestions2T(SuggestionCard,
                     [column(_ColNumber, _Technique,
                             TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                             TaskABB, TeamABB, TechnologyABB1, TechnologyABB2) | _Columns],
                     CardIds) :-
    member(SuggestionCard, [TeamABA, TechnologyABA1, TechnologyABA2]),
    is_not_empty(TaskABA),
    !,  % Case 2, upper row
    otherRowTime(TaskABB, TeamABB, TechnologyABB1, TechnologyABB2, Time),
    unbindT2([TeamABA, TechnologyABA1, TechnologyABA2], [UT1, UT2, UT3]),
    % debug_format('calling suggest2T: TaskABA=\'~w\', UT1=\'~w\', UT2=\'~w\', UT3=\'~w\'~n', [TaskABA, UT1, UT2, UT3]),
    suggest2T(SuggestionCard, TaskABA, UT1, UT2, UT3, Time, CardIds).
provideSuggestions2T(SuggestionCard,
                     [column(_ColNumber, _Technique,
                             TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                             TaskABB, TeamABB, TechnologyABB1, TechnologyABB2) | _Columns],
                     CardIds) :-
    member(SuggestionCard, [TeamABB, TechnologyABB1, TechnologyABB2]),
    is_not_empty(TaskABB),
    !,  % Case 2, lower row
    otherRowTime(TaskABA, TeamABA, TechnologyABA1, TechnologyABA2, Time),
    unbindT2([TeamABB, TechnologyABB1, TechnologyABB2], [UT1, UT2, UT3]),
    suggest2T(SuggestionCard, TaskABB, UT1, UT2, UT3, Time, CardIds).
provideSuggestions2T(SuggestionCard,
                     [column(_ColNumber, _Technique,
                             TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                             _TaskABB, _TeamABB, _TechnologyABB1, _TechnologyABB2) | _Columns],
                     CardIds) :-
    member(SuggestionCard, [TeamABA, TechnologyABA1, TechnologyABA2]),
    is_empty(TaskABA),
    !,  % Case 3, upper row
    suggest2T(SuggestionCard, TeamABA, TechnologyABA1, TechnologyABA2, CardIds).
provideSuggestions2T(SuggestionCard,
                     [column(_ColNumber, _Technique,
                             _TaskABA, _TeamABA, _TechnologyABA1, _TechnologyABA2,
                             TaskABB, TeamABB, TechnologyABB1, TechnologyABB2) | _Columns],
                     CardIds) :-
    member(SuggestionCard, [TeamABB, TechnologyABB1, TechnologyABB2]),
    is_empty(TaskABB),
    !,  % Case 3, lower row
    suggest2T(SuggestionCard, TeamABB, TechnologyABB1, TechnologyABB2, CardIds).
provideSuggestions2T(SuggestionCard, [_Column | Columns], CardIds) :-
    provideSuggestions2T(SuggestionCard, Columns, CardIds).

/*
  * otherRowTime(+Task, +Team, +Technology1, +Technology2, -Time)
  *   Time = week | synchronous | unknown
  */
otherRowTime(Task, _Team, _Technology1, _Technology2, unknown) :-
    is_empty(Task),
    !.
otherRowTime(Task, Team, Technology1, Technology2, Time) :-
    % if the row is fully specified
    % debug_format('enter oherRowTime2 ~w ~w ~w ~w~n', [Task, Team, Technology1, Technology2]),
    table_1(Task, Teams, Technologies, Time),
    member(Team, Teams),
    % debug_format('oherRowTime2 Technologies=~w~n', [Technologies]),
    ( is_empty(Technology1) ; member(Technology1, Technologies) ),
    ( is_empty(Technology2) ; member(Technology2, Technologies) ),
    !.
otherRowTime(Task, _Team, _Technology1, _Technology2, Time) :-
    % if the row is partially specified,
    % and for this task all clauses in table 1 specify the same value for Time (currently, 'week')
    findall(Tx, table_1(Task, _, _, Tx), BagOfTimes),
    sort(BagOfTimes, [Time]), % removes duplicates
    !.
otherRowTime(_Task, _Team, _Technology1, _Technology2, unknown).

/*
 * suggest2T/7
 * suggest2T(+SuggestionCard, +Task, +Team, +Technology1, +Technology2, +TimeOtherRow, -Teams)
 *
 * Deals with case 2:
 * If the suggestion card is placed on a team or technology slot, and a task card is
 *  present in the same column and row, suggestions are based on Table 1 in 3T_checks.docx
 *
 */
suggest2T(SuggestionCard, Task, Team, _Technology1, _Technology2, TimeOtherRow, Teams) :-
    /*
      * Case 2, suggestion card in the Team slot, TimeOtherRow is synchronous or unknown
    */
    Team == SuggestionCard,
    TimeOtherRow \= week,
    !,
    task_vs_teams_table_1(Task, Teams).
suggest2T(SuggestionCard, Task, Team, _Technology1, _Technology2, TimeOtherRow, Teams) :-
    /*
      * Case 2, suggestion card in the Team slot, TimeOtherRow is week
      *  Only clauses of table_1 (for this task) are considered whose Time is not 'week'
    */
    Team == SuggestionCard,
    TimeOtherRow == week,
    !,
    task_vs_teams_table_1(Task, [synchronous, unknown], Teams).  % not week
suggest2T(SuggestionCard, Task, _Team, Technology1, Technology2, TimeOtherRow, Technologies) :-
    /*
      * Case 2, suggestion card in the Technology1 slot, TimeOtherRow is synchronous or unknown:
      * If the suggestion card is placed on a technology slot, and a task card is
      *  present in the same column and row, suggestions are based on Table 1 in 3T_checks.docx
    */
    Technology1 == SuggestionCard,
    TimeOtherRow \= week,
    is_not_empty(Task),
    !,
    task_vs_technologies_table_1(Task, CollectedTechnologies),
    subtract(CollectedTechnologies, [Technology2], Technologies).
suggest2T(SuggestionCard, Task, _Team, Technology1, Technology2, TimeOtherRow, Technologies) :-
    /*
      * Case 2, suggestion card in the Technology1 slot, TimeOtherRow is week:
      * If the suggestion card is placed on a technology slot, and a task card is
      *  present in the same column and row, suggestions are based on Table 1 in 3T_checks.docx
      *  Only clauses of table_1 (for this task) are considered whose Time is not 'week'
    */
    Technology1 == SuggestionCard,
    TimeOtherRow == week,
    is_not_empty(Task),
    !,
    task_vs_technologies_table_1(Task, [synchronous, unknown], CollectedTechnologies),
    subtract(CollectedTechnologies, [Technology2], Technologies).
suggest2T(SuggestionCard, Task, _Team, Technology1, Technology2, TimeOtherRow, Technologies) :-
    /*
      * Case 2, suggestion card in the Technology2 slot, TimeOtherRow is synchronous or unknown:
      * If the suggestion card is placed on a technology slot, and a task card is
      *  present in the same column and row, suggestions are based on Table 1 in 3T_checks.docx
    */
    Technology2 == SuggestionCard,
    TimeOtherRow \= week,
    is_not_empty(Task),
    !,
    task_vs_technologies_table_1(Task, CollectedTechnologies),
    subtract(CollectedTechnologies, [Technology1], Technologies).
suggest2T(SuggestionCard, Task, _Team, Technology1, Technology2, TimeOtherRow, Technologies) :-
    /*
      * Case 2, suggestion card in the Technology2 slot, TimeOtherRow is week:
      * If the suggestion card is placed on a technology slot, and a task card is
      *  present in the same column and row, suggestions are based on Table 1 in 3T_checks.docx
      *  Only clauses of table_1 (for this task) are considered whose Time is not 'week'
    */
    Technology2 == SuggestionCard,
    TimeOtherRow == week,
    is_not_empty(Task),
    !,
    task_vs_technologies_table_1(Task, [synchronous, unknown], CollectedTechnologies),
    subtract(CollectedTechnologies, [Technology1], Technologies).
suggest2T(_SuggestionCard, Task, Team, Technology1, Technology2, _TimeOtherRow, []) :-
    debug_format('suggest2T/7.5 - catchall - should never happen - Task=~w Team=~w Technology1=~w Technology2=~w~n', [Task, Team, Technology1, Technology2]).

/*
 * suggest2T/5
 * suggest2T(+SuggestionCard, +Team, +Technology1, +Technology25, -Suggestions)
 *
 * Deals with case 3:
      * If the suggestion card is placed on a team or technology slot, and a task card is not
      *  present in the same column and row, suggestions are based on Table 2 in 3T_checks.
 *
 */
suggest2T(SuggestionCard, Team, Technology1, Technology2, CollectedTeams) :-
    /*
      * Case 3.1:
      * If the suggestion card is placed on a team slot, and a task card is not
      *  present in the same column and row, and cards are present in
      *  both technology slots in the same column and row,
      * Then suggested team cards are based on those rows of Table 2 in 3T_checks.docx
      *  that specify both the technologies.
    */
    Team == SuggestionCard,
    is_not_empty(Technology1),
    is_not_empty(Technology2),
    !,
    findall(Tx, team_vs_2techs(Technology1, Technology2, Tx), CollectedTeams).
suggest2T(SuggestionCard, Team, Technology1, Technology2, CollectedTeams) :-
    /*
      * Case 3.2:
      * If the suggestion card is placed on a team slot, and a task card is not
      *  present in the same column and row, and cards are present in
      *  the technology1 slot (but not in the technology2 slot) in the same column and row,
      * Then suggested team cards are based on those rows of Table 2 in 3T_checks.docx
      *  that specify the technology in the technology1 slot.
    */
    Team == SuggestionCard,
    is_not_empty(Technology1),
    is_empty(Technology2),
    !,
    findall(Tx, team_vs_1tech(Technology1, Tx), CollectedTeams).
suggest2T(SuggestionCard, Team, Technology1, Technology2, CollectedTeams) :-
    /*
      * Case 3.3:
      * If the suggestion card is placed on a team slot, and a task card is not
      *  present in the same column and row, and cards are present in
      *  the technology2 slot (but not in the technology1 slot) in the same column and row,
      * Then suggested team cards are based on those rows of Table 2 in 3T_checks.docx
      *  that specify the technology in the technology2 slot.
    */
    Team == SuggestionCard,
    is_not_empty(Technology2),
    is_empty(Technology1),
    !,
    findall(Tx, team_vs_1tech(Technology2, Tx), CollectedTeams).
suggest2T(SuggestionCard, Team, Technology1, Technology2, []) :-
    /*
      * Case 3.4:
      * If the suggestion card is placed on a team slot, and a task card is not
      *  present in the same column and row, and no cards are present in
      *  the technology slots in the same column and row,
      * Then no suggestion can be issued.
    */
    Team == SuggestionCard,
    is_empty(Technology2),
    is_empty(Technology1),
    !.
suggest2T(SuggestionCard, Team, Technology1, Technology2, CollectedTechs) :-
    /*
      * Case 3.5:
      * If the suggestion card is placed on technology1 slot, and a task card is not
      *  present in the same column and row, and cards are present both in the
      *  technology2 slot and in the team slot in the same column and row,
      * Then suggested technology cards are those in the row of Table 2 in 3T_checks.docx
      *  that specifies the team card, except the technology already present.
    */
    Technology1 == SuggestionCard,
    is_not_empty(Team),
    is_not_empty(Technology2),
    !,
    table_2(Team, Techs),
    subtract(Techs, [Technology2], CollectedTechs).
suggest2T(SuggestionCard, Team, Technology1, Technology2, CollectedTechs) :-
    /*
      * Case 3.6:
      * If the suggestion card is placed on technology2 slot, and a task card is not
      *  present in the same column and row, and cards are present both in the
      *  technology1 slot and in the team slot in the same column and row,
      * Then suggested technology cards are those in the row of Table 2 in 3T_checks.docx
      *  that specifies the team card, except the technology already present.
    */
    Technology2 == SuggestionCard,
    is_not_empty(Team),
    is_not_empty(Technology1),
    !,
    table_2(Team, Techs),
    subtract(Techs, [Technology1], CollectedTechs).
suggest2T(SuggestionCard, Team, Technology1, Technology2, CollectedTechs) :-
    /*
      * Case 3.7:
      * If the suggestion card is placed on technology1 slot, and a task card is not
      *  present in the same column and row, and a card is present in the team slot,
      *  and no card is present in the technology2 slot in the same column and row,
      * Then suggested technology cards are all those in the row of Table 2 in 3T_checks.docx
      *  that specifies the team card already present in the same column and row.
    */
    Technology1 == SuggestionCard,
    is_not_empty(Team),
    is_empty(Technology2),
    !,
    table_2(Team, CollectedTechs).
suggest2T(SuggestionCard, Team, Technology1, Technology2, CollectedTechs) :-
    /*
      * Case 3.8:
      * If the suggestion card is placed on technology2 slot, and a task card is not
      *  present in the same column and row, and a card is present in the team slot,
      *  and no card is present in the technology1 slot in the same column and row,
      * Then the suggested technology cards are all those in the row of Table 2 in 3T_checks.docx
      *  that specifies the team card already present in the same column and row.
    */
    Technology2 == SuggestionCard,
    is_not_empty(Team),
    is_empty(Technology1),
    !,
    table_2(Team, CollectedTechs).
suggest2T(SuggestionCard, Team, Technology1, Technology2, []) :-
    /*
      * Case 3.9:
      * If the suggestion card is placed on the technology1 slot, and a task card is not
      *  present in the same column and row, and no cards are present in the team and
      *  the technology1 slots in the same column and row,
      * Then no suggestion can be issued.
    */
    Technology1 == SuggestionCard,
    is_empty(Team),
    is_empty(Technology2),
    !.
suggest2T(SuggestionCard, Team, Technology1, Technology2, []) :-
    /*
      * Case 3.10:
      * If the suggestion card is placed on the technology2 slot, and a task card is not
      *  present in the same column and row, and no cards are present in the team and
      *  the technology1 slots in the same column and row,
      * Then no suggestion can be issued.
    */
    Technology2 == SuggestionCard,
    is_empty(Team),
    is_empty(Technology1),
    !.
suggest2T(SuggestionCard, Team, Technology1, Technology2, CollectedTechs) :-
    /*
      * Case 3.11:
      * If the suggestion card is placed on the technology1 slot, and a task card is not
      *  present in the same column and row, and no card is present in the team slot, and
      *  a card is present in the technology2 slots in the same column and row,
      * Then the suggestion is based on those rows of table 2 that include technology2.
    */
    Technology1 == SuggestionCard,
    is_empty(Team),
    is_not_empty(Technology2),
    !,
    findall(TechX, (table_2(_, Techs), member(Technology2, Techs), member(TechX, Techs)), UnsortedAllTechs),
    sort(UnsortedAllTechs, SortedAllTechs),
    subtract(SortedAllTechs, [Technology2], CollectedTechs).
suggest2T(SuggestionCard, Team, Technology1, Technology2, CollectedTechs) :-
    /*
      * Case 3.12:
      * If the suggestion card is placed on the technology2 slot, and a task card is not
      *  present in the same column and row, and no card is present in the team slot, and
      *  a card is present in the technology1 slots in the same column and row,
      * Then the suggestion is based on those rows of table 2 that include technology1.
    */
    Technology2 == SuggestionCard,
    is_empty(Team),
    is_not_empty(Technology1),
    !,
    findall(TechX, (table_2(_, Techs), member(Technology1, Techs), member(TechX, Techs)), UnsortedAllTechs),
    sort(UnsortedAllTechs, SortedAllTechs),
    subtract(SortedAllTechs, [Technology1], CollectedTechs).
suggest2T(_SuggestionCard, Team, Technology1, Technology2, []) :-
    debug_format('suggest2T/5.4 - catchall - should never happen - Team=~w Technology1=~w Technology2=~w~n', [Team, Technology1, Technology2]).
