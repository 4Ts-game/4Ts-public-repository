% -*- Mode: Prolog -*-

:- module(httpDriver, [server/1, semanticCheck/2, buildBoard/2]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(sgml)).
:- use_module(kbCommons).
:- use_module(kbBase, [isTechnique/1,
                       checkBoard_base/5]).
:- use_module(kbAdvanced, [checkBoard_advanced/5]).

% The predicate server(+Port) starts the server. It simply creates a
% number of Prolog threads and then returns to the toplevel, so you can
% (re-)load code, debug, etc.
server(Port) :-
	http_server(http_dispatch, [port(Port)]).

% Declare a handler, binding an HTTP path to a predicate.
:- http_handler(root(ping), doPing, []).
:- http_handler(root(semantic_check_Unity), doCheck_Unity, []).
:- http_handler(root(toXML_Unity), doToXML, []).

/* The implementation of /. The single argument provides the request
details, which we ignore for now. Our task is to write a CGI-Document:
a number of name: value -pair lines, followed by two newlines, followed
by the document content, The only obligatory header line is the
Content-type: <mime-type> header.
Printing can be done using any Prolog printing predicate, but the
format-family is the most useful. See format/2.   */

/*** HTTP HANDLERS
  *
  * doPing/1
  * doCheck_Unity/1
   * doToXML/1
  *
  ***/

doPing(Request) :-
	format('Content-type: text/xml~n~n'),
	format('<?xml version="1.0"?>~n', []),
	format('<!DOCTYPE ping-response >~n', []),
	format('<ping-response>~n', []),
	format('  <system-is-active />~n', []),
	version(Version, Data),
	member(peer(IP), Request),
    format('  <system-version>~s</system-version>~n', [Version]),
	format('  <system-date>~s</system-date>~n', [Data]),
	format('  <client-ip>~w</client-ip>~n', [IP]),
	format('</ping-response>~n', []).

doCheck_Unity(Request) :-
    format('Content-type: text/xml~n~n'),
	format('<?xml version="1.0"?>~n', []),
	format('<!DOCTYPE kb-response SYSTEM "kb_out.dtd">~n', []),
    %% debug_format('calling buildBoard with Request=~p~n', [Request]),
    member(request_uri(Params), Request),
    member(path(Path), Request),
    member(search(Search), Request),
 	saveContent('http_test.pl', [request_uri(Params),path(Path),search(Search)]),
	buildBoard(Request, Board),
	saveContent('board_test.pl', Board),
    %% debug_format('buildBoard returns ~w~n', [Board]),
	semanticCheck(Board, Result),
	saveContent('request_test.pl', Result),
	format_response(Result, ResponseXMLString),
	writeln(ResponseXMLString),
	toXML(Request, XML_Request),
	logger(Request, XML_Request, ResponseXMLString).

doToXML(Request) :-
	format('Content-type: text/xml~n~n'),
	format('<?xml version="1.0"?>~n', []),
	format('<!DOCTYPE board SYSTEM "kb_in.dtd">~n', []),
	toXML(Request, XML_Doc),
	format('~w~n', [XML_Doc]).

/*** Utilities ***/

saveContent(FileName, Term) :-
	open(FileName, write, Stream, [close_on_abort(true)]),
	write_term(Stream, Term, [max_depth(0), quoted(true), fullstop(true), nl(true)]),
	close(Stream).


format_response(Result, XMLString) :-
	with_output_to(string(XMLString), format_response(Result)).
format_response(result(InconsistentSlots,InconsistentDetails,IncompleteCardInSlots,Suggestions)) :-
	format('<kb-response>~n'),
	format('  <inconsistent-slots>~n'),
	listXML('slot', InconsistentSlots),
	format('  </inconsistent-slots>~n'),
	format('  <inconsistent-details>~n'),
	listPredicateXML(['code', 'detail'], InconsistentDetails),
	format('  </inconsistent-details>~n'),
	format('  <missing-cards>~n'),
	listXML('card-in-slot', IncompleteCardInSlots),
	format('  </missing-cards>~n'),
	format('  <suggested-cards>~n'),
	listXML('card', Suggestions),
	format('  </suggested-cards>~n'),
	format('</kb-response>~n').
format_response(resultAdvanced(InconsistentSlots,InconsistentDetails,IncompleteCardInSlots,Suggestions)) :-
	format('<kb-response>~n'),
	format('  <inconsistent-slots>~n'),
	listXML('slot', InconsistentSlots),
	format('  </inconsistent-slots>~n'),
	format('  <inconsistent-details>~n'),
	listPredicateXML(['code', 'detail'], InconsistentDetails),
	format('  </inconsistent-details>~n'),
	format('  <missing-cards>~n'),
	listXML('card-in-slot', IncompleteCardInSlots),
	format('  </missing-cards>~n'),
	format('  <suggested-cards>~n'),
	listXML('card', Suggestions),
	format('  </suggested-cards>~n'),
	format('</kb-response>~n').

listXML(_Label, []).
listXML('card-in-slot', [slotCardPair(Slot, Card)| T]) :-
	format('    <card-in-slot>~n      <card>~w</card>~n      <slot>~w</slot>~n    </card-in-slot>~n',
	       [Card, Slot]),
	listXML('card-in-slot', T).
listXML(Label, [H|T]) :-
	format('    <~s>~s</~s>~n', [Label, H, Label]),
	listXML(Label, T).

listPredicateXML(_Labels, []).
listPredicateXML(Labels, [H|T]) :-
	H =.. [_|Args],
	listPredicateArgsXML(Labels, Args),
	listPredicateXML(Labels, T).

listPredicateArgsXML([],[]).
listPredicateArgsXML([Label|RestLabels], [Arg|RestArgs]) :-
	format('    <~s>~s</~s>~n', [Label, Arg, Label]),
	listPredicateArgsXML(RestLabels, RestArgs).

logger(Request, Board, Result) :-
	get_time(TS),
	format_time(string(Name), 'logs/log_%Y%m%d.xml', TS),
	format_time(atom(DT), '%F %T', TS),
	member(peer(IP), Request),
	open(Name, append, OStream, [close_on_abort(true), create([all])]),
	logger(IP, OStream, DT, Board, Result),
	close(OStream, [force(true)]).

logger(IP, OStream, DT, Board, Result) :-
	format(OStream, '\c
	      <logEntry>~n\c
	      <timestamp>~w</timestamp>~n\c
          <client-ip>~w</client-ip>~n\c
	      ~s~n\c
	      ~s~n\c
	      </logEntry>~n~n',
	      [DT, IP, Board, Result]).



/****************************************************************
 *
 *   Board representation in version 5.00 and later:
 *
 *   board(Level, CheckCompleteness, ListOfColumns).
 *      Level is either 'base' or 'advanced'
 *      CheckCompleteness is boolean
 *      ListOfColumns contains exactly 4 columns.
 *
 *   Each column:
 *   column(ColNumber, Technique,
 *          TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
 *          TaskABB, TeamABB, TechnologyABB1, TechnologyABB2).
 *
 **/

buildBoard(Request, Board) :-
	http_parameters(Request,
			[level(Level, [default(base)]),
             checkCompleteness(CheckCompleteness, [default(false)]),
             'C1-CSW-TECHNIQUE'(Technique1, [default('')]),
			 'C2-CSW-TECHNIQUE'(Technique2, [default('')]),
			 'C3-CSW-TECHNIQUE'(Technique3, [default('')]),
			 'C4-CSW-TECHNIQUE'(Technique4, [default('')]),
			 'C1-ABA-CSS-TASK'(Task1a,  [default('')]),
			 'C1-ABA-CSS-TEAM'(Team1a,  [default('')]),
			 'C2-ABA-CSS-TASK'(Task2a,  [default('')]),
			 'C2-ABA-CSS-TEAM'(Team2a,  [default('')]),
			 'C3-ABA-CSS-TASK'(Task3a,  [default('')]),
			 'C3-ABA-CSS-TEAM'(Team3a,  [default('')]),
			 'C4-ABA-CSS-TASK'(Task4a,  [default('')]),
			 'C4-ABA-CSS-TEAM'(Team4a,  [default('')]),
			 'C1-ABA-CSS-TEC1'(Tecnology1a1,  [default('')]),
			 'C1-ABA-CSS-TEC2'(Tecnology1a2,  [default('')]),
			 'C2-ABA-CSS-TEC1'(Tecnology2a1,  [default('')]),
			 'C2-ABA-CSS-TEC2'(Tecnology2a2,  [default('')]),
			 'C3-ABA-CSS-TEC1'(Tecnology3a1,  [default('')]),
			 'C3-ABA-CSS-TEC2'(Tecnology3a2,  [default('')]),
			 'C4-ABA-CSS-TEC1'(Tecnology4a1,  [default('')]),
			 'C4-ABA-CSS-TEC2'(Tecnology4a2,  [default('')]),
			 'C1-ABB-CSS-TASK'(Task1b,  [default('')]),
			 'C1-ABB-CSS-TEAM'(Team1b,  [default('')]),
			 'C2-ABB-CSS-TASK'(Task2b,  [default('')]),
			 'C2-ABB-CSS-TEAM'(Team2b,  [default('')]),
			 'C3-ABB-CSS-TASK'(Task3b,  [default('')]),
			 'C3-ABB-CSS-TEAM'(Team3b,  [default('')]),
			 'C4-ABB-CSS-TASK'(Task4b,  [default('')]),
			 'C4-ABB-CSS-TEAM'(Team4b,  [default('')]),
			 'C1-ABB-CSS-TEC1'(Tecnology1b1,  [default('')]),
			 'C1-ABB-CSS-TEC2'(Tecnology1b2,  [default('')]),
			 'C2-ABB-CSS-TEC1'(Tecnology2b1,  [default('')]),
			 'C2-ABB-CSS-TEC2'(Tecnology2b2,  [default('')]),
			 'C3-ABB-CSS-TEC1'(Tecnology3b1,  [default('')]),
			 'C3-ABB-CSS-TEC2'(Tecnology3b2,  [default('')]),
			 'C4-ABB-CSS-TEC1'(Tecnology4b1,  [default('')]),
			 'C4-ABB-CSS-TEC2'(Tecnology4b2,  [default('')])
			]),
	toCardType(Technique1, Technique1Type),

	toCardType(Task1a, Task1aType),
	toCardType(Team1a, Team1aType),
	toCardType(Tecnology1a1, Tecnology1a1Type),
	toCardType(Tecnology1a2, Tecnology1a2Type),

	toCardType(Task1b, Task1bType),
	toCardType(Team1b, Team1bType),
	toCardType(Tecnology1b1, Tecnology1b1Type),
	toCardType(Tecnology1b2, Tecnology1b2Type),

	toCardType(Technique2, Technique2Type),

	toCardType(Task2a, Task2aType),
	toCardType(Team2a, Team2aType),
	toCardType(Tecnology2a1, Tecnology2a1Type),
	toCardType(Tecnology2a2, Tecnology2a2Type),

	toCardType(Task2b, Task2bType),
	toCardType(Team2b, Team2bType),
	toCardType(Tecnology2b1, Tecnology2b1Type),
	toCardType(Tecnology2b2, Tecnology2b2Type),

	toCardType(Technique3, Technique3Type),

	toCardType(Task3a, Task3aType),
	toCardType(Team3a, Team3aType),
	toCardType(Tecnology3a1, Tecnology3a1Type),
	toCardType(Tecnology3a2, Tecnology3a2Type),

	toCardType(Task3b, Task3bType),
	toCardType(Team3b, Team3bType),
	toCardType(Tecnology3b1, Tecnology3b1Type),
	toCardType(Tecnology3b2, Tecnology3b2Type),

	toCardType(Technique4, Technique4Type),

	toCardType(Task4a, Task4aType),
	toCardType(Team4a, Team4aType),
	toCardType(Tecnology4a1, Tecnology4a1Type),
	toCardType(Tecnology4a2, Tecnology4a2Type),

	toCardType(Task4b, Task4bType),
	toCardType(Team4b, Team4bType),
	toCardType(Tecnology4b1, Tecnology4b1Type),
	toCardType(Tecnology4b2, Tecnology4b2Type),

	Col1 =.. [
		  column,
		  1,
		  Technique1Type,
		  Task1aType,
		  Team1aType,
		  Tecnology1a1Type,
		  Tecnology1a2Type,
		  Task1bType,
		  Team1bType,
		  Tecnology1b1Type,
		  Tecnology1b2Type
		 ],
	Col2 =.. [
		  column,
		  2,
		  Technique2Type,
		  Task2aType,
		  Team2aType,
		  Tecnology2a1Type,
		  Tecnology2a2Type,
		  Task2bType,
		  Team2bType,
		  Tecnology2b1Type,
		  Tecnology2b2Type
		 ],
	Col3 =.. [
		  column,
		  3,
		  Technique3Type,
		  Task3aType,
		  Team3aType,
		  Tecnology3a1Type,
		  Tecnology3a2Type,
		  Task3bType,
		  Team3bType,
		  Tecnology3b1Type,
		  Tecnology3b2Type
		 ],
	Col4 =.. [
		  column,
		  4,
		  Technique4Type,
		  Task4aType,
		  Team4aType,
		  Tecnology4a1Type,
		  Tecnology4a2Type,
		  Task4bType,
		  Team4bType,
		  Tecnology4b1Type,
		  Tecnology4b2Type
		 ],
	Board =.. [
		   board,
		   Level,
		   CheckCompleteness,
           [Col1, Col2, Col3, Col4]]. % list of columns

toXML(Request, XML_Doc) :-
	http_parameters(Request,
			[level(Level, [default(base)]),
             checkCompleteness(CheckCompleteness, [default(false)]),
			 'context'(Context, [default('')]),
			 'objectives'(Objectives, [default('')]),
			 'content'(Content, [default('')]),
			 'C1-CSW-TECHNIQUE'(Technique1, [default('')]),
				%'C1-CSW-PHASE'(Phase1, [default('')]),
			 'C2-CSW-TECHNIQUE'(Technique2, [default('')]),
				%'C2-CSW-PHASE'(Phase2, [default('')]),
			 'C3-CSW-TECHNIQUE'(Technique3, [default('')]),
				%'C3-CSW-PHASE'(Phase3, [default('')]),
			 'C4-CSW-TECHNIQUE'(Technique4, [default('')]),
				%'C4-CSW-PHASE'(Phase4, [default('')]),
			 'C1-ABA-CSS-TASK'(Task1a,  [default('')]),
			 'C1-ABA-CSS-TEAM'(Team1a,  [default('')]),
			 'C2-ABA-CSS-TASK'(Task2a,  [default('')]),
			 'C2-ABA-CSS-TEAM'(Team2a,  [default('')]),
			 'C3-ABA-CSS-TASK'(Task3a,  [default('')]),
			 'C3-ABA-CSS-TEAM'(Team3a,  [default('')]),
			 'C4-ABA-CSS-TASK'(Task4a,  [default('')]),
			 'C4-ABA-CSS-TEAM'(Team4a,  [default('')]),
			 'C1-ABA-CSS-TEC1'(Tecnology1a1,  [default('')]),
			 'C1-ABA-CSS-TEC2'(Tecnology1a2,  [default('')]),
			 'C2-ABA-CSS-TEC1'(Tecnology2a1,  [default('')]),
			 'C2-ABA-CSS-TEC2'(Tecnology2a2,  [default('')]),
			 'C3-ABA-CSS-TEC1'(Tecnology3a1,  [default('')]),
			 'C3-ABA-CSS-TEC2'(Tecnology3a2,  [default('')]),
			 'C4-ABA-CSS-TEC1'(Tecnology4a1,  [default('')]),
			 'C4-ABA-CSS-TEC2'(Tecnology4a2,  [default('')]),
			 'C1-ABB-CSS-TASK'(Task1b,  [default('')]),
			 'C1-ABB-CSS-TEAM'(Team1b,  [default('')]),
			 'C2-ABB-CSS-TASK'(Task2b,  [default('')]),
			 'C2-ABB-CSS-TEAM'(Team2b,  [default('')]),
			 'C3-ABB-CSS-TASK'(Task3b,  [default('')]),
			 'C3-ABB-CSS-TEAM'(Team3b,  [default('')]),
			 'C4-ABB-CSS-TASK'(Task4b,  [default('')]),
			 'C4-ABB-CSS-TEAM'(Team4b,  [default('')]),
			 'C1-ABB-CSS-TEC1'(Tecnology1b1,  [default('')]),
			 'C1-ABB-CSS-TEC2'(Tecnology1b2,  [default('')]),
			 'C2-ABB-CSS-TEC1'(Tecnology2b1,  [default('')]),
			 'C2-ABB-CSS-TEC2'(Tecnology2b2,  [default('')]),
			 'C3-ABB-CSS-TEC1'(Tecnology3b1,  [default('')]),
			 'C3-ABB-CSS-TEC2'(Tecnology3b2,  [default('')]),
			 'C4-ABB-CSS-TEC1'(Tecnology4b1,  [default('')]),
			 'C4-ABB-CSS-TEC2'(Tecnology4b2,  [default('')])
			]),

	format(string(XML_Doc),
	       '<board level=\'~w\' >
	      <context>~w</context>
	      <goals>
	      <goal>~w</goal>
	      </goals>
	      <content>~w</content>
	      <flags>
	      <flag>checkCompleteness=~w</flag>
	      </flags>
	      <columns>
	      <column id="1">
	      <csw-technique>
	      <technique>~w</technique>
	      </csw-technique>
	      <aba-css-task>~w</aba-css-task>
	      <aba-css-team>~w</aba-css-team>
	      <aba-css-tec1>~w</aba-css-tec1>
	      <aba-css-tec2>~w</aba-css-tec2>
	      <abb-css-task>~w</abb-css-task>
	      <abb-css-team>~w</abb-css-team>
	      <abb-css-tec1>~w</abb-css-tec1>
	      <abb-css-tec2>~w</abb-css-tec2>
	      </column>
	      <column id="2">
	      <csw-technique>
	      <technique>~w</technique>
	      </csw-technique>
	      <aba-css-task>~w</aba-css-task>
	      <aba-css-team>~w</aba-css-team>
	      <aba-css-tec1>~w</aba-css-tec1>
	      <aba-css-tec2>~w</aba-css-tec2>
	      <abb-css-task>~w</abb-css-task>
	      <abb-css-team>~w</abb-css-team>
	      <abb-css-tec1>~w</abb-css-tec1>
	      <abb-css-tec2>~w</abb-css-tec2>
	      </column>
	      <column id="3">
	      <csw-technique>
	      <technique>~w</technique>
	      </csw-technique>
	      <aba-css-task>~w</aba-css-task>
	      <aba-css-team>~w</aba-css-team>
	      <aba-css-tec1>~w</aba-css-tec1>
	      <aba-css-tec2>~w</aba-css-tec2>
	      <abb-css-task>~w</abb-css-task>
	      <abb-css-team>~w</abb-css-team>
	      <abb-css-tec1>~w</abb-css-tec1>
	      <abb-css-tec2>~w</abb-css-tec2>
	      </column>
	      <column id="4">
	      <csw-technique>
	      <technique>~w</technique>
	      </csw-technique>
	      <aba-css-task>~w</aba-css-task>
	      <aba-css-team>~w</aba-css-team>
	      <aba-css-tec1>~w</aba-css-tec1>
	      <aba-css-tec2>~w</aba-css-tec2>
	      <abb-css-task>~w</abb-css-task>
	      <abb-css-team>~w</abb-css-team>
	      <abb-css-tec1>~w</abb-css-tec1>
	      <abb-css-tec2>~w</abb-css-tec2>
	      </column>
	      </columns>
	      </board>
	      ',
	      [Level, Context, Objectives, Content, CheckCompleteness,
	       Technique1,
	       Task1a, Team1a, Tecnology1a1, Tecnology1a2,
	       Task1b, Team1b, Tecnology1b1, Tecnology1b2,
	       Technique2,
	       Task2a, Team2a, Tecnology2a1, Tecnology2a2,
	       Task2b, Team2b, Tecnology2b1, Tecnology2b2,
	       Technique3,
	       Task3a, Team3a, Tecnology3a1, Tecnology3a2,
	       Task3b, Team3b, Tecnology3b1, Tecnology3b2,
	       Technique4,
	       Task4a, Team4a, Tecnology4a1, Tecnology4a2,
	       Task4b, Team4b, Tecnology4b1, Tecnology4b2]
	      ).


/***	semanticCheck ***/

semanticCheck(Board, resultAdvanced(InconsistentCardPositions, InconsistentCardDetails, IncompleteCardPositions, Suggestions)) :-
    Board = board(advanced, _, _),
    !,
	checkBoard_advanced(Board, InconsistentCardPositions, InconsistentCardDetails, IncompleteCardPositions, Suggestions).
semanticCheck(Board, result(InconsistentCardPositions, InconsistencyDetails, IncompleteCardPositions, Suggestions)) :-
	checkBoard_base(Board, InconsistentCardPositions, InconsistencyDetails, IncompleteCardPositions, Suggestions).
