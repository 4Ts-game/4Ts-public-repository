% -*- Mode: Prolog -*-

:- module(suggestionsAdvanced, [provideSuggestions_advanced/2]).

:- use_module(kbAdvanced).
:- use_module(kbCommons).
:- use_module(kbBase).


/* provideSuggestions_advanced/2(+Columns, -CardIds)
  *
  * If a suggestion card is present, provide a list of card instances for that slot
*/
provideSuggestions_advanced(Columns, CardIds) :-
  %debug_format('suggestions advanced for: ~n~p~n', [Columns]),
  unbindSlotsWithSuggestion(SuggestionVar, SuggestionType, Columns, UnboundColumns),
  %debug_format('suggestion type: ~p~n', [SuggestionType]),
  (var(SuggestionType),
    CardIds = [];
    %debug_format('unbound suggestion slot: ~n~p~nsuggestion type: ~p~n', [UnboundColumns,SuggestionType]),
    getAllInstancesOfType(SuggestionType, AllInstances),
    %debug_format('suggestion candidates: ~n~p~n', [AllInstances]),
    %debug_format('findall on ~n~p~n', [findall(SuggestionVar, getValidInstance(UnboundColumns, SuggestionVar, AllInstances), RawSuggestions)]),
    findall(SuggestionVar, getValidInstance(UnboundColumns, SuggestionVar, AllInstances), RawSuggestions),
    %debug_format('raw suggestions: ~n~p~n', [RawSuggestions]),
    mapToCardInstances(RawSuggestions, CardIds)
  ), !.

/* getAllInstancesOfType
  * return all instances of a given type (task, team or technology)
*/
getAllInstancesOfType(Type, AllInstances) :-
  Pattern =.. [Type, Instance, _],
  findall(Instance, Pattern, AllInstances).

/* getValidInstance
  * checks which members of the provided list make for valid (but possibly incomplete) boards
*/
getValidInstance(Columns, UnboundSlot, Instances) :-
  member(UnboundSlot, Instances),
  checkInconsistencies(true, Columns, [], Inconsistencies, _InconsistencyDetails),
  %debug_format('inconsistencies in getValidInstance ~n~p~n~p~n', [Inconsistencies, InconsistencyDetails]),
  length(Inconsistencies, 0).

/* mapToCardInstances
  * duplicate of what's in suggestionsBase at this point; but it may change one day
  * maps a cart type (task, team, technology) to the first card instance of that type
*/
mapToCardInstances([], []).
mapToCardInstances([CardType | CardTypes], [InstanceId | InstanceIds]) :-
  card_instances(CardType, [InstanceId | _]), % always takes the first instance - BEWARE
  mapToCardInstances(CardTypes, InstanceIds).

