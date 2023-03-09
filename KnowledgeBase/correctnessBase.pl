%% -*- Mode: Prolog -*-

:- module(correctnessBase, [checkCorrectness_base/3]).

:- use_module(kbCommons).
:- use_module(kbBase).
:- dynamic wrongSlot/1.


/*
  * checkCorrectness_base(+ListOfColumns, -ListOfInconsistentCardPositions, -ListOfInconsistencyDetails)
  *
  * First, correctness is checked across expanded board patterns: if no inconsistecies
  *   are found, the predicate succeeds without other checks.
  * If check across patterns fails, then the slot containing the wrong card
  *   is identified as follows:
  *   considering only the N non-empty slots, cards are removed one
  *   at a time, obtaining N reduced board representations; at least one must be
  *   correct, as the gaming policies allow that a card be put in an empty slot
  *   only if starting from a correct board configuration: so removing the wrong
  *   card should yield an accepptable board state.
  *   The name of the slot containing the wrong card is therefore returned.
  *
  */
checkCorrectness_base(ListOfColumns, [], []) :-
    controlCard(SuggestionCard, 'call-for-suggestion'),
    removeSuggestionCard(SuggestionCard, ListOfColumns, L1),
    % If the following succeeds, no inconcistencies are found
    % debug_format('correctness check across patterns starts with ListOfColumns=~w~n', [ListOfColumns]),
	unbindSlots(L1, L2),
	Pattern =.. [boardPatternExpanded, L2],
	findall(Pattern, Pattern, Bag),
	length(Bag, Len),
	% debug_format('   Bag Length = ~d~n', [Len]),
	Len > 0,
    !.
checkCorrectness_base(ListOfColumns, [WrongSlotResult], [ErrorMsg]) :-
    controlCard(SuggestionCard, 'call-for-suggestion'),
    removeSuggestionCard(SuggestionCard, ListOfColumns, L1),
	unbindSlots(L1, L2),
    % debug_format('correctness check across patterns fails with L2=~w~n', [L2]),
    % debug_format('   trying with reduced board configurations~n', []),
    findall((US, L3), tentativeUnbindSlot(L2, US, L3), Bag),
    % length(Bag, N),
    % debug_format('   #UnboundSlots = ~d, UnboundSlots = ~w~n', [N, Bag]),
    tryReducedListOfColumns(Bag, WrongSlot),
    convertSlotNameIntoGenericError(WrongSlot, WrongSlotResult, ErrorMsg).
checkCorrectness_base(ListOfColumns, ['C1-CSW-TECHNIQUE'], [ErrorMsg]) :-
    convertSlotNameIntoGenericError('C1-CSW-TECHNIQUE', _, ErrorMsg),
    debug_format('checkCorrectness_base catchall with ListOfColumns=~w. Should never happen.~n', [ListOfColumns]).

tryReducedListOfColumns([], WrongSlot) :-
    WrongSlot = 'MULTIPLE',
    debug_format('cannot find reduced LoC in tryReducedListOfColumns (multiple invalid slots); returning top level error instead~n', []).
tryReducedListOfColumns([(Slot, LoC) | Rest], WrongSlot) :-
    %debug_format('try reducing (~w, ~w)~n', [Slot, LoC]),
    tryReducedListOfColumn(LoC, Result),
    ( Result = false ->
      tryReducedListOfColumns(Rest, WrongSlot)
      ;
      ( %% debug_format('found reduced LoC: (~w, ~w)~n', [Slot, LoC]),
        WrongSlot = Slot )
    ).

tryReducedListOfColumn(LoC, true) :-
    %% test LoC across patterns
    %% debug_format('  try reducing ~w~n', [LoC]),
	Pattern =.. [boardPatternExpanded, LoC],
    %% debug_format('     tryReducedListOfColumn: ~p~n', [Pattern]),
	findall(Pattern, Pattern, Bag),
	length(Bag, Len),
	%% debug_format('     Bag Length = ~d~n', [Len]),
	Len > 0.
tryReducedListOfColumn(_LoC, false).

/*
 * removeSuggestionCard/3
 * removeSuggestionCard(+SuggestionCardCode, +Columns, -Columns1)
 *   If a suggestion card is specified in any slot of any column in Columns,
 *   the same slot in the corresponding column of Columns1 is emptied.
 *   All other slots of Columns1 contain the same cards as Columns.
 */
removeSuggestionCard(_SuggestionCard, [], []).
removeSuggestionCard(SuggestionCard, [Col | Cols], [Col1 | Cols1]) :-
	removeSuggestionCardCol(SuggestionCard, Col, Col1),
    %debug_format('   rsc Col=~w, Col1=~w~n', [Col, Col1]),
	removeSuggestionCard(SuggestionCard, Cols, Cols1).

/*
 * removeSuggestionCardCol/3
 * removeSuggestionCardCol(+SuggestionCardCode, +Column, -Column1)
 *   if a suggestion card is specified in any slot of Column, the same slot
 *   in Column1 is empty. All other slots of Column1 contain the same cards as
 *   Column.
 *   Only the first occurrence of the suggestion card is removed (no more than one
 *   should be allowed by the user interface).
 */
removeSuggestionCardCol(SuggestionCard,
    column(ColNumber, SuggestionCard,
        TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
        TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
    column(ColNumber, '',
        TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
        TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
 	!.
removeSuggestionCardCol(SuggestionCard,
    column(ColNumber, Technique,
        SuggestionCard, TeamABA, TechnologyABA1, TechnologyABA2,
        TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
    column(ColNumber, Technique,
        '', TeamABA, TechnologyABA1, TechnologyABA2,
        TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
	!.
removeSuggestionCardCol(SuggestionCard,
    column(ColNumber, Technique,
        TaskABA, SuggestionCard, TechnologyABA1, TechnologyABA2,
        TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
    column(ColNumber, Technique,
        TaskABA, '', TechnologyABA1, TechnologyABA2,
        TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
	!.
removeSuggestionCardCol(SuggestionCard,
    column(ColNumber, Technique,
        TaskABA, TeamABA, SuggestionCard, TechnologyABA2,
        TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
    column(ColNumber, Technique,
        TaskABA, TeamABA, '', TechnologyABA2,
        TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
	!.
removeSuggestionCardCol(SuggestionCard,
    column(ColNumber, Technique,
        TaskABA, TeamABA, TechnologyABA1, SuggestionCard,
        TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
    column(ColNumber, Technique,
        TaskABA, TeamABA, TechnologyABA1, '',
        TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
	!.
removeSuggestionCardCol(SuggestionCard,
    column(ColNumber, Technique,
        TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
        SuggestionCard, TeamABB, TechnologyABB1, TechnologyABB2),
    column(ColNumber, Technique,
        TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
        '', TeamABB, TechnologyABB1, TechnologyABB2)) :-
	!.
removeSuggestionCardCol(SuggestionCard,
    column(ColNumber, Technique,
        TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
        TaskABB, SuggestionCard, TechnologyABB1, TechnologyABB2),
    column(ColNumber, Technique,
        TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
        TaskABB, '', TechnologyABB1, TechnologyABB2)) :-
	!.
removeSuggestionCardCol(SuggestionCard,
    column(ColNumber, Technique,
        TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
        TaskABB, TeamABB, SuggestionCard, TechnologyABB2),
    column(ColNumber, Technique,
        TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
        TaskABB, TeamABB, '', TechnologyABB2)) :-
	!.
removeSuggestionCardCol(SuggestionCard,
    column(ColNumber, Technique,
        TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
        TaskABB, TeamABB, TechnologyABB1, SuggestionCard),
    column(ColNumber, Technique,
        TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
        TaskABB, TeamABB, TechnologyABB1, '')) :-
	!.
removeSuggestionCardCol(_SuggestionCard, Col, Col).


/*
 * tentativeUnbindSlot/3
 *
 * tentativeUnbindSlot(+ListOfColumns, -Slot, -ListCleaned)
 *
 * ListCleaned is a copy of ListOfColumns where each non empty slot (at a time) has been changed into unbound.
 * Slots are made unbound one at a time, from col 1 to col 4 and from row 1 to row 2.
 * The purpose is to identify the (sole) card that causes an inconsistency: this predicate
 * sould be evaluated repeatedly across backtracking, each time cheching the correctness of ListCleaned, stopping
 * when a correct configuration is encountered.
 *
 * for testing purposes ('202' in col 1 is wrong):
 *
 ?- correctnessBase:tentativeUnbindSlot([column(1,'001','102','201','306','','','202','',''),
                                         column(2,'','106','203','302','310','108','206','309','310'),
                                         column(3,'002','101','203','310','308','108','206','309','310'),
                                         column(4,'','','','','','','','','')], US, UL).
 *
 */
tentativeUnbindSlot([Col1, Col2, Col3, Col4], UnboundSlot, [CleanedCol1, Col2, Col3, Col4]) :-
    tentativeUnbindSlotInCol(Col1, UnboundSlot, CleanedCol1).
tentativeUnbindSlot([Col1, Col2, Col3, Col4], UnboundSlot, [Col1, CleanedCol2, Col3, Col4]) :-
    tentativeUnbindSlotInCol(Col2, UnboundSlot, CleanedCol2).
tentativeUnbindSlot([Col1, Col2, Col3, Col4], UnboundSlot, [Col1, Col2, CleanedCol3, Col4]) :-
    tentativeUnbindSlotInCol(Col3, UnboundSlot, CleanedCol3).
tentativeUnbindSlot([Col1, Col2, Col3, Col4], UnboundSlot, [Col1, Col2, Col3, CleanedCol4]) :-
    tentativeUnbindSlotInCol(Col4, UnboundSlot, CleanedCol4).


tentativeUnbindSlotInCol(column(1, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C1-CSW-TECHNIQUE',
                column(1, _,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(Technique).
tentativeUnbindSlotInCol(column(1, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C1-ABA-CSS-TASK',
                column(1, Technique,
                	   _, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TaskABA).
tentativeUnbindSlotInCol(column(1, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C1-ABA-CSS-TEAM',
                column(1, Technique,
                	   TaskABA, _, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TeamABA).
tentativeUnbindSlotInCol(column(1, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C1-ABA-CSS-TEC1',
                column(1, Technique,
                	   TaskABA, TeamABA, _, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TechnologyABA1).
tentativeUnbindSlotInCol(column(1, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C1-ABA-CSS-TEC2',
                column(1, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, _,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TechnologyABA2).
tentativeUnbindSlotInCol(column(1, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C1-ABB-CSS-TASK',
                column(1, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       _, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TaskABB).
tentativeUnbindSlotInCol(column(1, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C1-ABB-CSS-TEAM',
                column(1, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, _, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TeamABB).
tentativeUnbindSlotInCol(column(1, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C1-ABB-CSS-TEC1',
                column(1, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, _, TechnologyABB2)) :-
    is_not_empty(TechnologyABB1).
tentativeUnbindSlotInCol(column(1, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C1-ABB-CSS-TEC2',
                column(1, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, _)) :-
    is_not_empty(TechnologyABB2).

tentativeUnbindSlotInCol(column(2, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C2-CSW-TECHNIQUE',
                column(2, _,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(Technique).
tentativeUnbindSlotInCol(column(2, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C2-ABA-CSS-TASK',
                column(2, Technique,
                	   _, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TaskABA).
tentativeUnbindSlotInCol(column(2, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C2-ABA-CSS-TEAM',
                column(2, Technique,
                	   TaskABA, _, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TeamABA).
tentativeUnbindSlotInCol(column(2, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C2-ABA-CSS-TEC1',
                column(2, Technique,
                	   TaskABA, TeamABA, _, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TechnologyABA1).
tentativeUnbindSlotInCol(column(2, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C2-ABA-CSS-TEC2',
                column(2, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, _,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TechnologyABA2).
tentativeUnbindSlotInCol(column(2, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C2-ABB-CSS-TASK',
                column(2, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       _, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TaskABB).
tentativeUnbindSlotInCol(column(2, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C2-ABB-CSS-TEAM',
                column(2, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, _, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TeamABB).
tentativeUnbindSlotInCol(column(2, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C2-ABB-CSS-TEC1',
                column(2, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, _, TechnologyABB2)) :-
    is_not_empty(TechnologyABB1).
tentativeUnbindSlotInCol(column(2, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C2-ABB-CSS-TEC2',
                column(2, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, _)) :-
    is_not_empty(TechnologyABB2).

tentativeUnbindSlotInCol(column(3, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C3-CSW-TECHNIQUE',
                column(3, _,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(Technique).
tentativeUnbindSlotInCol(column(3, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C3-ABA-CSS-TASK',
                column(3, Technique,
                	   _, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TaskABA).
tentativeUnbindSlotInCol(column(3, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C3-ABA-CSS-TEAM',
                column(3, Technique,
                	   TaskABA, _, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TeamABA).
tentativeUnbindSlotInCol(column(3, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C3-ABA-CSS-TEC1',
                column(3, Technique,
                	   TaskABA, TeamABA, _, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TechnologyABA1).
tentativeUnbindSlotInCol(column(3, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C3-ABA-CSS-TEC2',
                column(3, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, _,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TechnologyABA2).
tentativeUnbindSlotInCol(column(3, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C3-ABB-CSS-TASK',
                column(3, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       _, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TaskABB).
tentativeUnbindSlotInCol(column(3, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C3-ABB-CSS-TEAM',
                column(3, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, _, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TeamABB).
tentativeUnbindSlotInCol(column(3, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C3-ABB-CSS-TEC1',
                column(3, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, _, TechnologyABB2)) :-
    is_not_empty(TechnologyABB1).
tentativeUnbindSlotInCol(column(3, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C3-ABB-CSS-TEC2',
                column(3, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, _)) :-
    is_not_empty(TechnologyABB2).

tentativeUnbindSlotInCol(column(4, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C4-CSW-TECHNIQUE',
                column(4, _,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(Technique).
tentativeUnbindSlotInCol(column(4, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C4-ABA-CSS-TASK',
                column(4, Technique,
                	   _, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TaskABA).
tentativeUnbindSlotInCol(column(4, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C4-ABA-CSS-TEAM',
                column(4, Technique,
                	   TaskABA, _, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TeamABA).
tentativeUnbindSlotInCol(column(4, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C4-ABA-CSS-TEC1',
                column(4, Technique,
                	   TaskABA, TeamABA, _, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TechnologyABA1).
tentativeUnbindSlotInCol(column(4, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C4-ABA-CSS-TEC2',
                column(4, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, _,
                       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TechnologyABA2).
tentativeUnbindSlotInCol(column(4, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C4-ABB-CSS-TASK',
                column(4, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       _, TeamABB, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TaskABB).
tentativeUnbindSlotInCol(column(4, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C4-ABB-CSS-TEAM',
                column(4, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, _, TechnologyABB1, TechnologyABB2)) :-
    is_not_empty(TeamABB).
tentativeUnbindSlotInCol(column(4, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C4-ABB-CSS-TEC1',
                column(4, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, _, TechnologyABB2)) :-
    is_not_empty(TechnologyABB1).
tentativeUnbindSlotInCol(column(4, Technique,
		               TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		               TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
                'C4-ABB-CSS-TEC2',
                column(4, Technique,
                	   TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                       TaskABB, TeamABB, TechnologyABB1, _)) :-
    is_not_empty(TechnologyABB2).
