% -*- Mode: Prolog -*-

:- module(completenessBase, [checkCompleteness_base/2]).

:- use_module(kbCommons).
:- use_module(kbBase).



/*
   * checkCompleteness_base/2    ---    EXPORTED
   *
   * checkCompleteness_base(+Cols, -IncompleteCardPositionPairs)
   *
   * IncompleteCardPositionPairs is the set of incomlete slots (+ possible completion values)
   *   for the first (from left) incomplete column in Cols
   */
checkCompleteness_base(Cols, IncompleteCardPositionPairs) :-
	removeSuggestionCard(Cols, Cols1),
	checkCompleteness1(Cols1, IncompleteCardPositionPairs).

/*
 * checkCompleteness1/2
 *
 * checkCompleteness1(+Cols, -IncompleteCardPositionPairs)
 * Cols no longer contain any suggestion card
 *
 */

%checkCompleteness1(Cols, _):-
% 	debug_format('check completeness: ~w~n', [Cols]),
% 	fail.

/*  No technique cards are present in any column: inter-column completeness check succeeds with no feedback */
checkCompleteness1([Col1, Col2, Col3, Col4], []):-
	Col1 = column(1, T1, _, _, _, _, _, _, _, _),
	Col2 = column(2, T2, _, _, _, _, _, _, _, _),
	Col3 = column(3, T3, _, _, _, _, _, _, _, _),
	Col4 = column(4, T4, _, _, _, _, _, _, _, _),
	is_empty(T1),
	is_empty(T2),
	is_empty(T3),
	is_empty(T4),
	%debug_format('check completeness: all techniques are empty, check succeeds without feedback~n', []),
	!.

/*  At least one technique card is present in some column */
checkCompleteness1(ListOfColumns, IncompleteCardPositionPairs) :-
	unbindSlots(ListOfColumns, L1),
	Pattern =.. [boardPatternExpanded, L1],
	findall(Pattern, Pattern, Bag),
	%% length(Bag, Len),
	%% debug_format('+++ checkCompleteness/2: L1 = ~w~nBag Length = ~d~n~n', [L1, Len]),
	/* Incomplete slots are those var elements of L1 that are unified to a ground, non-empty value
	   in some bag element
	   Example:
	   L1 = [a, b, _1, _2, _3]
	   Bag = [[a, b, c, _4, ''], [a, b, d, _5, '']]
	   then only the third element of L1 causes incompleteness: elements that are unbound, or empty,
	   both in L1 and in Bag do not cause incompleteness.
	   This condition is checked in checkCompleteness/3.
	*/
	checkCompletenessAcrossPatterns(L1, Bag, FullIncompleteCardPositionPairs, 0, RightMostCompleteColumn),
%	filterOnlyLeftmostIncompleteColumn(FullIncompleteCardPositionPairs, FilteredCardPositionPairs),		% this is the original implementation by Luigi, which relies on a specific order returned for incomplete columns
	keepOnlyRightmostIncompleteColumn(FullIncompleteCardPositionPairs, FilteredCardPositionPairs, RightMostCompleteColumn),	% new implementation ignores the order of returned incomplete columns [Minollo]
	sort(FilteredCardPositionPairs, IncompleteCardPositionPairs).
	%IncompleteCardPositionPairs = [slotCardPair(Slot, _) | _],
	%atom_chars(Slot, ['C', Col | _]),
	%debug_format('leftmost column: ~w~n', [Col]). % to be removed

% Remove column entries which are to the left of the righmost reported column, as the presence of a reported column to the right
% shows that there are complete paths possible for the columns to its left
keepOnlyRightmostIncompleteColumn([], [], _).
keepOnlyRightmostIncompleteColumn([slotCardPair(Slot, Card) | Others], FilteredPairs, RightMostCompleteColumn) :-
	atom_chars(Slot, ['C', ColAtom | _]),
	atom_number(ColAtom, Col),
	keepOnlyRightmostIncompleteColumn(Others, FilteredOthers, RightMostCompleteColumn),
	(Col > RightMostCompleteColumn -> FilteredPairs = [slotCardPair(Slot, Card) | FilteredOthers]; FilteredPairs = FilteredOthers).

% Deprecated
filterOnlyLeftmostIncompleteColumn([], []).
filterOnlyLeftmostIncompleteColumn([slotCardPair(Slot, Card) | Others], [slotCardPair(Slot, Card) | Filtered]) :-
	atom_chars(Slot, ['C', Col | _]),
	%%debug_format('first completeness feedback points to column ~w~n', [Col]),
	extractPairs(Others, Col, Filtered).

% Deprecated 
extractPairs([], _, []).
extractPairs([slotCardPair(Slot, Card) | Rest], Col, [slotCardPair(Slot, Card) | Rest1]) :-
	atom_chars(Slot, ['C', Col | _]),
	!,
	extractPairs(Rest, Col, Rest1).
extractPairs([_ | Rest], Col, Rest1) :-
	extractPairs(Rest, Col, Rest1).

/*
 * checkCompletenessAcrossPatterns/5
 *
 * checkCompletenessAcrossPatterns(+BoardColumns, +Bag, -IncompleteCardPositionPairs, +CurrentRightMostCompleteColumn, -RightMostCompleteColumn)
    for each boardPatternExpanded(FourColumns) element in Bag,
     compare BoardColumns with FourColumns:
      the names of the slots in BoardColumns that are unbound,
      and that have a corresponding slot in FourColumns bound to some card value,
      form new pairs slotCardPair(<slot_name>, <card_value>)
      and the pair is added to IncompleteCardPositionPairs.
     The counter to the RightMostCompleteColumn is necessary to know up to which column we have a combination of complete values.
*/
checkCompletenessAcrossPatterns(_BoardColumns, [], [], C, C).
checkCompletenessAcrossPatterns(BoardColumns,
				  [ boardPatternExpanded(FourColumns) | RestOfBag ],
				  IncompleteCardPositionPairs, RightMostCompleteColumnIn, RightMostCompleteColumnMax) :-
	lookColumnsForUnbounds(BoardColumns, FourColumns, TheseIncompleteCardPositionPairs, 0, RightMostCompleteColumnThis),
	(RightMostCompleteColumnThis > RightMostCompleteColumnIn -> RightMostCompleteColumnNow = RightMostCompleteColumnThis; RightMostCompleteColumnNow = RightMostCompleteColumnIn),
	append(TheseIncompleteCardPositionPairs, RestOfIncompleteCardPositionPairs, IncompleteCardPositionPairs),
	checkCompletenessAcrossPatterns(BoardColumns, RestOfBag, RestOfIncompleteCardPositionPairs, RightMostCompleteColumnNow, RightMostCompleteColumnMax).


lookColumnsForUnbounds([], [], [], C, C).
lookColumnsForUnbounds([], L, [], C, C) :-
	length(L, Len),
	Len > 0,
	!,
	/* 1st arg is empty, and 2nd arg is not. Should never happen. */
	debug_format('In lookColumnsForUnbounds 1st arg is [], and 2nd arg is ~w. Should never happen.~n', [L]).
lookColumnsForUnbounds(L, [], [], C, C) :-
	length(L, Len),
	Len > 0,
	!,
	/* 2nd arg is empty, and 1st arg is not. Should never happen. */
	debug_format('In lookColumnsForUnbounds 1st arg is ~w, and 2nd arg is []. Should never happen.~n', [L]).
lookColumnsForUnbounds([Column | Columns], [CompleteColumn | CompleteColumns], Pairs, CurrentCompleteColumn, RightMostCompleteColumn) :-
	lookColumnForUnbounds(Column, CompleteColumn, ThesePairs),
    ( length(ThesePairs, 0) ->          % if this column has no incomplete slots...
      Column =.. [_, NewCompleteColumn | _],
      lookColumnsForUnbounds(Columns, CompleteColumns, Pairs, NewCompleteColumn, RightMostCompleteColumn) ;  % then check next column(s)
      RightMostCompleteColumn = CurrentCompleteColumn,
      ThesePairs = Pairs                 % otherwise there is no need to look at next columns
	).

/*
  *   Each column:
  *   column(ColNumber, Technique,
  *          TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
  *          TaskABB, TeamABB, TechnologyABB1, TechnologyABB2).
*/
lookColumnForUnbounds(Column, CompleteColumn, Pairs) :-
	Column =.. [_, ColN | L1],	              % strip first two elements ('column', <colnumber>)
	CompleteColumn =.. [_, ColN | L2],		  % same as above
	qualifiedSlotNames(ColN, ListOfNames),
	%% debug_format('+++ Calling lookListForUnbounds/4: ListOfNames = ~w~nL1 = ~w~nL2 = ~w~n~n',
	%% 	     [ListOfNames, L1, L2]),
	lookListForUnbounds(ListOfNames, L1, L2, Pairs).


/*
    lookListForUnbounds/4

    lookListForUnbounds(+ListOfNames, +L1, +L2, -Pairs)
        where:
    ListOfNames is ['C<n>-CSW-TECHNIQUE',
    		      'C<n>-ABA-CSS-TASK', 'C<n>-ABA-CSS-TEAM', 'C<n>-ABA-CSS-TEC1', 'C<n>-ABA-CSS-TEC2',
    		      'C<n>-ABB-CSS-TASK', 'C<n>-ABB-CSS-TEAM', 'C<n>-ABB-CSS-TEC1', 'C<n>-ABB-CSS-TEC2']
                where <n> is in 1..4
    L1 is [Technique, TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                      TaskABB, TeamABB, TechnologyABB1, TechnologyABB2]
          from a possibly incomplete column of the board
    L2 is [Technique, TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
                      TaskABB, TeamABB, TechnologyABB1, TechnologyABB2]
          from a complete column of boardPatternExpanded
          ListOfNames, L1 and L2 always contain the same number of elements
    Pairs is a list of slotCardPair(Name, CompleteSlot) elements, where
        Name refers to those elements of L1 that are unbound, and
        CompleteSLot refers to the corresponding element in L2
*/
%lookListForUnbounds(ListOfNames, L1, L2, _) :-
%    debug_format('lookListForUnbounds/4: ListOfNames = ~w~nL1 = ~w~nL2 = ~w~n~n', [ListOfNames, L1, L2]),
%    fail.
lookListForUnbounds(_ListOfNames, [], [], []).
lookListForUnbounds(_ListOfNames, [], L, []) :-
	length(L, Len),
	Len > 0,
	!,
	/* 1st arg is empty, and 2nd arg is not. Should never happen. */
	debug_format('In lookListForUnbounds.a 1st arg is [], and 2nd arg is ~w. Should never happen.~n', [L]).
lookListForUnbounds(_ListOfNames, L, [], []) :-
	length(L, Len),
	Len > 0,
	!,
	/* 2nd arg is empty, and 1st arg is not. Should never happen. */
	debug_format('In lookListForUnbounds.b 1st arg is ~w, and 2nd arg is []. Should never happen.~n', [L]).
lookListForUnbounds([_Name | Names], [Slot | Slots], [CompleteSlot | CompleteSlots], Rest) :-
	var(Slot),
	var(CompleteSlot),    % non dovrebbe mai accadere: boardPatternExpanded dovrebbe essere completamente istanziato
	!,
	debug_format('In lookListForUnbounds.c CompleteSlot is unbound. Should never happen.~n', []),
    lookListForUnbounds(Names, Slots, CompleteSlots, Rest).
lookListForUnbounds([_Name | Names], [Slot | Slots], [CompleteSlot | CompleteSlots], Rest) :-
	var(Slot),
    is_empty(CompleteSlot),
	!,
	lookListForUnbounds(Names, Slots, CompleteSlots, Rest).
lookListForUnbounds([Name | Names], [Slot | Slots], [CompleteSlot | CompleteSlots],
					[slotCardPair(Name, CompleteSlot) | Rest]) :-
	var(Slot),
	%nonvar(CompleteSlot),   % pleonastica
	!,
    %debug_format('In lookListForUnbounds.e incomplete slot ~w gets bound to value ~w.~n', [Name, CompleteSlot]),
	lookListForUnbounds(Names, Slots, CompleteSlots, Rest).
lookListForUnbounds([_Name | Names], [Slot | Slots], [CompleteSlot | CompleteSlots], Rest) :-
	nonvar(Slot),
	var(CompleteSlot),
	!,
	/* Should only happen in columns where no technique card is present. */ % ??? incomprensibile, probabilmente errato
	debug_format('In lookListForUnbounds.f Slot is ~w, and CompleteSLot is free. Should only happen in columns where no technique card is present.~n', [Slot]),
	lookListForUnbounds(Names, Slots, CompleteSlots, Rest).
lookListForUnbounds([_Name | Names], [SameValue | Slots], [SameValue | CompleteSlots], Rest) :-
	/* if both Slot and CompleteSlot are bound, they must be bound to the same value. */
	!,
	lookListForUnbounds(Names, Slots, CompleteSlots, Rest).
lookListForUnbounds([_Name | Names], [Slot | Slots], [CompleteSlot | CompleteSlots], Rest) :-
	nonvar(Slot),
	nonvar(CompleteSlot),
	/* Slot and CompleteSlot are bound to different values. Should never happen.*/
	debug_format('In lookListForUnbounds.h args are both bound, but to different values: Slot is ~w, and CompleteSLot is ~w. Should never happen.~n', [Slot, CompleteSlot]),
	lookListForUnbounds(Names, Slots, CompleteSlots, Rest).
