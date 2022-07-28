% -*- Mode: Prolog -*-

:- module(kbBase,
          [card_instances/2,
           checkBoard_base/4,
           technique/5,
           isTechnique/1,
           boardPatternExpanded/1,
           boardPattern/1]).

:- use_module(kbCommons).
:- use_module(correctnessBase, [checkCorrectness_base/2]).
:- use_module(completenessBase, [checkCompleteness_base/2]).
:- use_module(suggestionsBase, [provideSuggestions_base/2]).

:- dynamic boardPatternExpanded/1.

/*
  *
  * Card reference
  *
  * https://docs.google.com/spreadsheets/d/1vbKRP_QAa5aDEWWEEgJravq-R_m_ZlHpkclr3hm7-lw/edit?usp=drive_web&ouid=113162760941048132931
  *
*/

%%%
%%% card_instances_base/2: type_id, list_of_instance_ids
%%% This set of cards is added to the card_instances_commons set if level is base
%%%

card_instances_base('001', ['22']).	% Jigsaw 1
card_instances_base('002', ['37']).	% Jigsaw 2
card_instances_base('003', ['38']).	% Peer review 1
card_instances_base('004', ['39']).	% Peer review 2
card_instances_base('005', ['40']).	% Peer review 3
card_instances_base('006', ['41', '178']).	% Case study 1
card_instances_base('007', ['42', '179']).	% Case study 2
card_instances_base('008', ['23']).	% Pyramid for list 1
card_instances_base('009', ['43']).	% Pyramid for list 2
card_instances_base('010', ['44']).	% Pyramid for list 3
card_instances_base('011', ['45']).	% Pyramid for problem solving 1
card_instances_base('012', ['46']).	% Pyramid for problem solving 2
card_instances_base('013', ['47']).	% Pyramid for problem solving 3
card_instances_base('014', ['48', '83']).	% Discussion 1
card_instances_base('015', ['49', '173']).	% Discussion assignment 2
card_instances_base('016', ['50', '174']).	% Discussion artefact 2
card_instances_base('017', ['51', '175']).	% Discussion report 2
card_instances_base('018', ['52', '176']).	% Role play 1
card_instances_base('019', ['53', '177']).	% Role play 2

/*
  *
  * card_instances(Type_id, ListOfInstance_ids)
  *
  * This predicate represents the union of card instances available at both
  *  levels 'card_instances_commons' and of card instances available at the base
  *  level 'card_instances_base'.
  *
  * card_instances/2 should be used only within the XXXbase modules.
  * It is likely that for XXXadvanced modules a different version of this predicate
  * wil be defined.
  *
  */
card_instances(Type_id, ListOfInstance_ids) :-
    card_instances_commons(Type_id, ListOfInstance_ids).
card_instances(Type_id, ListOfInstance_ids) :-
    card_instances_base(Type_id, ListOfInstance_ids).

%
% tecnique/5: type_id, name, sequence, n_phase, n_max_phases
%    0 < id < 100
technique('001', 'JIGSAW - PHASE I (EXPERT GROUPS)', jigsaw, 1, 2).
technique('002', 'JIGSAW - PHASE II (JIGSAW GROUPS)', jigsaw, 2, 2).
technique('003', 'PEER REVIEW - PHASE I', peerreview, 1, 3).
technique('004', 'PEER REVIEW - PHASE II', peerreview, 2, 3).
technique('005', 'PEER REVIEW - PHASE III', peerreview, 3, 3).
technique('006', 'CASE STUDY - PHASE I', casestudy, 1, 2).
technique('007', 'CASE STUDY - PHASE II', casestudy, 2, 2).
technique('008', 'PYRAMID (FOR LIST PREPARATION) - PHASE I', pyramidforlist, 1, 3).
technique('009', 'PYRAMID (FOR LIST PREPARATION) - PHASE II', pyramidforlist, 2, 3).
technique('010', 'PYRAMID (FOR LIST PREPARATION) - PHASE III', pyramidforlist, 3, 3).
technique('011', 'PYRAMID (FOR PROBLEM SOLVING) - PHASE I', pyramidforproblem, 1, 3).
technique('012', 'PYRAMID (FOR PROBLEM SOLVING) - PHASE II', pyramidforproblem, 2, 3).
technique('013', 'PYRAMID (FOR PROBLEM SOLVING) - PHASE III', pyramidforproblem, 3, 3).
technique('014', 'DISCUSSION - PHASE I (ALL CASES)', discussion, 1, 2).
technique('015', 'DISCUSSION (TOWARDS ASSIGNMENT) - PHASE II', discussion, 2, 2).
technique('016', 'DISCUSSION (TOWARDS ARTEFACT) - PHASE II', discussion, 2, 2).
technique('017', 'DISCUSSION (TOWARDS REPORT) - PHASE II', discussion, 2, 2).
technique('018', 'ROLE PLAY - PHASE I', roleplay, 1, 2).
technique('019', 'ROLE PLAY - PHASE II', roleplay, 2, 2).

isTechnique('') :- !.
isTechnique(Card) :-
	controlCard(Card, _),
	!.
isTechnique(Card) :-
	technique(Card, _, _, _, _),
	!.
isTechnique(_Card) :-
	fail.

/**********
  * checkBoard_base (+board,
  *                   -inconsistentSlots,
  *                   -incompltetePairs,
  *                   -suggestionPairs)
  *
*/

%% checkBoard_base(Board, _, _, _) :-
%%     debug_format('Entering checkBoard_base: ~p~n', [Board]),
%%     fail.
checkBoard_base(board(_Level, true, ListOfColumns),	% check completeness too
		         InconsistentCardPositions, IncompleteCardPositionPairs, []) :-
	checkCorrectness_base(ListOfColumns, InconsistentCardPositions),
	(
	 length(InconsistentCardPositions, 0) -> % if there are no inconsistecies, then check completeness
	 checkCompleteness_base(ListOfColumns, IncompleteCardPositionPairs)
     ;
     IncompleteCardPositionPairs = []
	),
	!.     %  no suggestions if check completeness has been requested

checkBoard_base(board(_Level, false, ListOfColumns),	% do not check completeness
		 InconsistentCardPositions, [], Suggestions) :-
	checkCorrectness_base(ListOfColumns, InconsistentCardPositions),
	!,
	(
	 length(InconsistentCardPositions, 0) -> % if there are no inconsistecies, then provide possible suggestions
     provideSuggestions_base(ListOfColumns, Suggestions)
     ;
     Suggestions = []
    ).
checkBoard_base(board(_Level, UnknownCheckCompleteness, _ListOfColumns), [], [], [] ) :-
	/*** this should never happen! ***/
	debug_format('- - Catchall in checkBoard_base - Unknown value for checkCompleteness flag: =\'~w\' - -',
		     [UnknownCheckCompleteness]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* BOARD PATTERNS */

/**
  *
  *   Board representation in version 5.00 (see httpDriver.pl):
  *
  *   board(+Level, +CheckCompleteness, -ListOfColumns).
  *
  *   Each column:
  *   column(ColNumber, Technique,
  *          TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
  *          TaskABB, TeamABB, TechnologyABB1, TechnologyABB2).
  *
  *          ColNumber is integer. All other fields are atoms.
  *
  **/

/****
  * Week duration of Techniques:
  *
  * Jigsaw      --> 3
  * Peer Review --> 4
  * Case Study  --> 2 if f2f,
  *             --> 3 if mixed       <-- added Dec 07, 2019
  *             --> 4 if online
  * Pyramid     --> 4
  * Discussion  --> 2
  * Role Play   --> 2
  ****/

/****
  * ALL POSSIBLE TECHNIQUE PATTERNS over 4 weeks :
  * Jigsaw starting 1st week
  * Jigsaw starting 2nd week
  * Peer Review
  * Case Study ftf starting 1st week, no techniques in weeks 3 and 4
  * Case Study ftf starting 2nd week
  * Case Study ftf starting 3rd week, no techniques in week 1 and 2
  * Case Study ftf starting 1st week, Case Study ftf starting 3rd week        <-- added Nov 29, 2019
  * Case Study online
  * Case Study mixed starting 1st week       <-- added Dec 07, 2019
  * Case Study mixed starting 2nd week       <-- added Dec 07, 2019
  * Pyramid
  * Discussion starting 1st week, no techniques in weeks 3 and 4
  * Discussion starting 2nd week
  * Discussion starting 3rd week, no techniques in week 1 and 2
  * Discussion starting 1st week, Discussion starting 3rd week                <-- added Nov 29, 2019
  * Role Play starting 1st week, no techniques in weeks 3 and 4
  * Role Play starting 2nd week
  * Role Play starting 3rd week, no techniques in week 1 and 2
  * Role Play starting 1st week, Role Play starting 3rd week                  <-- added Nov 29, 2019
  * Case Study ftf staring 1st week, Discussion starting 3rd week
  * Discussion staring 1st week, Case Study ftf starting 3rd week
  * Case Study ftf staring 1st week, Role Play ftf starting 3rd week
  * Role Play ftf staring 1st week, Case Study ftf starting 3rd week
  * Discussion staring 1st week, Role Play ftf starting 3rd week
  * Role Play ftf staring 1st week, Discussion starting 3rd week
  ****/


/* Jigsaw starting 1st week */
boardPattern( [ column(1, '001',
		       '102', '201', '306', '',
		       '', '', '', ''),
		column(2, '',
		       '106', '203', '302', or('301', '310'),
		       '108', '206', or('305', '309'), ''),
		column(3, '002',
		       '101', '203', '308', or('301', '310'),
		       '108', '206', or('305', '309'), ''),
		column(4, '',
		       _Task4ABA, _Team4ABA, _Technology4ABA1, _Technology4ABA2,
		       _Task4ABB, _Team4ABB, _Technology4ABB1, _Technology4ABB2) ]).

/* Jigsaw starting 2nd week - removed after skype of 23/12/2020
boardPattern( [ column(1, '',	% ftf
		       _Task1ABA, _Team1ABA, _Technology1ABA1, _Technology1ABA2,
		       _Task1ABB, _Team1ABB, _Technology1ABB1, _Technology1ABB2),
		column(2, '001',
		       '102', '201', '306', '',
		       '', '', '', ''),
		column(3, '',
		       '106', '203', '302', or('301', '310'),
		       '108', '206', or('305', '309'), ''),
		column(4, '002',
		       '101', '203', '308', or('301', '310'),
		       '108', '206', or('305', '309'), '') ]).
*/

/* Peer Review */
boardPattern( [ column(1, '003',
		       '102', '201', '306', '',
		       '', '', '', ''),
		column(2, '',
		       '112', or('201', '202', '203'), '311', or('301', '310'),
		       '', '', '', ''),
		column(3, '004',
		       '105', or('201', '202', '203'), or('301', '308'), '',
		       '', '', '', ''),
		column(4, '005',
		       '112', or('201', '202', '203'), '311', or('301', '310'),
		       '108', '206', or('305', '309'), '') ]).

/* Case Study ftf */
/* Case Study ftf starting 1st week, no techniques in weeks 3 and 4 */
boardPattern( [ column(1, '006',
		       '102', '201', '306', '',
		       '109', or('202', '203'), '310', ''),
		column(2, '007',
		       '102', '201', '306', '',
		       '113', or('203', '206'), '310', ''),
		column(3, '',
		       _Task3ABA, _Team3ABA, _Technology3ABA1, _Technology3ABA2,
		       _Task3ABB, _Team3ABB, _Technology3ABB1, _Technology3ABB2),
		column(4, '',
		       _Task4ABA, _Team4ABA, _Technology4ABA1, _Technology4ABA2,
		       _Task4ABB, _Team4ABB, _Technology4ABB1, _Technology4ABB2) ]).

/* Case Study ftf starting 2nd week, no techniques in weeks 1 and 4 - removed after skype of 23/12/2020
boardPattern( [ column(1, '',
		       _Task1ABA, _Team1ABA, _Technology1ABA1, _Technology1ABA2,
		       _Task1ABB, _Team1ABB, _Technology1ABB1, _Technology1ABB2),
		column(2, '006',
		       '102', '201', '306', '',
		       '109', or('202', '203'), '310', ''),
		column(3, '007',
		       '102', '201', '306', '',
		       '113', or('203', '206'), '310', ''),
		column(4, '',
		       _Task4ABA, _Team4ABA, _Technology4ABA1, _Technology4ABA2,
		       _Task4ABB, _Team4ABB, _Technology4ABB1, _Technology4ABB2) ]).
*/
/* Case Study ftf starting 3rd week, no techniques in week 1 and 2 - removed after skype of 23/12/2020
boardPattern( [ column(1, '',
		       _Task1ABA, _Team1ABA, _Technology1ABA1, _Technology1ABA2,
		       _Task1ABB, _Team1ABB, _Technology1ABB1, _Technology1ABB2),
		column(2, '',
		       _Task2ABA, _Team2ABA, _Technology2ABA1, _Technology2ABA2,
		       _Task2ABB, _Team2ABB, _Technology2ABB1, _Technology2ABB2),
		column(3, '006',
		       '102', '201', '306', '',
		       '109', or('202', '203'), '310', ''),
		column(4, '007',
		       '102', '201', '306', '',
		       '113', or('203', '206'), '310', '') ]).
*/
/* Case Study online + online, starting 1st week, lasting 4 weeks */
boardPattern( [ column(1, '006',
		       '102', '201', '306', '',
		       '', '', '', ''),
		column(2, '',
		       '109', or('202', '203'), '301', '',
		       '', '', '', ''),
		column(3, '007',
		       '102', '201', '306', '',
		       '', '', '', ''),
		column(4, '',
		       '113', or('203', '206'), '301', '',
		       '', '', '', '') ]).
/*  Case Study ftf + online starting 1st week       <-- added Dec 07, 2019 */
boardPattern( [ column(1, '006',
		       '102', '201', '306', '',
		       '109', or('202', '203'), '310', ''),
		column(2, '007',
		       '102', '201', '306', '',
		       '', '', '', ''),
		column(3, '',
          '113', or('203', '206'), '301', '',
          '', '', '', ''),
		column(4, '',
		       _Task4ABA, _Team4ABA, _Technology4ABA1, _Technology4ABA2,
		       _Task4ABB, _Team4ABB, _Technology4ABB1, _Technology4ABB2) ]).
/*  Case Study ftf + online starting 2nd week - removed after skype of 23/12/2020
boardPattern( [ column(1, '',
       _Task4ABA, _Team4ABA, _Technology4ABA1, _Technology4ABA2,
       _Task4ABB, _Team4ABB, _Technology4ABB1, _Technology4ABB2),
  column(2, '006',
	       '102', '201', '306', '',
	       '109', or('202', '203'), '310', ''),
	column(3, '007',
	        '102', '201', '306', '',
	        '', '', '', ''),
	column(4, '',
         '113', or('203', '206'), '301', '',
         '', '', '', '')	 ]).
*/
/*  Case Study online + ftf starting 1st week       <-- added Dec 07, 2019 */
boardPattern( [ column(1, '006',
		       '102', '201', '306', '',
		       '', '', '', ''),
		column(2, '',
		       '109', or('202', '203'), '301', '',
		       '', '', '', ''),
		column(3, '007',
          '102', '201', '306', '',
          '113', or('203', '206'), '310', ''),
		column(4, '',
		       _Task4ABA, _Team4ABA, _Technology4ABA1, _Technology4ABA2,
		       _Task4ABB, _Team4ABB, _Technology4ABB1, _Technology4ABB2) ]).
/*  Case Study online + ftf starting 2nd week - removed after skype of 23/12/2020
boardPattern( [ column(1, '',
       _Task4ABA, _Team4ABA, _Technology4ABA1, _Technology4ABA2,
       _Task4ABB, _Team4ABB, _Technology4ABB1, _Technology4ABB2),
    column(2, '006',
		       '102', '201', '306', '',
		       '', '', '', ''),
		column(3, '',
		       '109', or('202', '203'), '301', '',
		       '', '', '', ''),
		column(4, '007',
          '102', '201', '306', '',
          '113', or('203', '206'), '310', '') ]).
*/
/* Pyramid for list preparation */
boardPattern( [ column(1, '008', % indiv. --> pair --> plenary
		       '102', '201', '306', '',
		       '', '', '', ''),
		column(2, '',
		       '104', '201', or('301', '308'), or('', '301', '308'),
		       '', '', '', ''),
		column(3, '009',
		       '104', '202', or('301', '308'), or('', '301', '308'),
		       '', '', '', ''),
		column(4, '010',
		       '104', '206', or('301', '308'), or('', '301', '308'),
		       '', '', '', '') ]).
boardPattern( [ column(1, '008', % pair --> small group --> plenary
		       '102', '201', '306', '',
		       '', '', '', ''),
		column(2, '',
		       '104', '202', or('301', '308'), or('', '301', '308'),
		       '', '', '', ''),
		column(3, '009',
		       '104', '203', or('301', '308'), or('', '301', '308'),
		       '', '', '', ''),
		column(4, '010',
		       '104', '206', or('301', '308'), or('', '301', '308'),
		       '', '', '', '') ]).

/* Pyramid for problem solving */
boardPattern( [ column(1, '011', % indiv. --> pair --> plenary
		       '102', '201', '306', '',
		       '', '', '', ''),
		column(2, '',
		       '109', '201', or('301', '308'), or('', '301', '308'),
		       '', '', '', ''),
		column(3, '012',
		       '109', '202', or('301', '308'), or('', '301', '308'),
		       '', '', '', ''),
		column(4, '013',
		       '109', '206', or('301', '308'), or('', '301', '308'),
		       '', '', '', '') ]).
boardPattern( [ column(1, '011', % pair --> small group --> plenary
		       '102', '201', '306', '',
		       '', '', '', ''),
		column(2, '',
		       '109', '202', or('301', '308'), or('', '301', '308'),
		       '', '', '', ''),
		column(3, '009',
		       '109', '203', or('301', '308'), or('', '301', '308'),
		       '', '', '', ''),
		column(4, '010',
		       '109', '206', or('301', '308'), or('', '301', '308'),
		       '', '', '', '') ]).


/* Discussion starting 1st week, no techniques in weeks 3 and 4 */
boardPattern( [ column(1, '014',
		       '103', '201', '307', '',
		       '113', '206', or('310', '305'), ''),
		column(2, '015', % towards assignment
		       '107', or('201', '202', '203'), '311', or('310','301'),
		       '', '', '', ''),
		column(3, '',
		       _Task3ABA, _Team3ABA, _Technology3ABA1, _Technology3ABA2,
		       _Task3ABB, _Team3ABB, _Technology3ABB1, _Technology3ABB2),
		column(4, '',
		       _Task4ABA, _Team4ABA, _Technology4ABA1, _Technology4ABA2,
		       _Task4ABB, _Team4ABB, _Technology4ABB1, _Technology4ABB2) ]).
boardPattern( [ column(1, '014',
		       '103', '201', '307', '',
		       '113', '206', or('310', '305'), ''),
		column(2, '016', % towards artefact
		       '112', or('201', '202', '203'), '311', or('310','301'),
		       '', '', '', ''),
		column(3, '',
		       _Task3ABA, _Team3ABA, _Technology3ABA1, _Technology3ABA2,
		       _Task3ABB, _Team3ABB, _Technology3ABB1, _Technology3ABB2),
		column(4, '',
		       _Task4ABA, _Team4ABA, _Technology4ABA1, _Technology4ABA2,
		       _Task4ABB, _Team4ABB, _Technology4ABB1, _Technology4ABB2) ]).
boardPattern( [ column(1, '014',
		       '103', '201', '307', '',
		       '113', '206', or('310', '305'), ''),
		column(2, '017', % towards report
		       '101', or('201', '202', '203'), '308', or('310','301'),
		       '', '', '', ''),
		column(3, '',
		       _Task3ABA, _Team3ABA, _Technology3ABA1, _Technology3ABA2,
		       _Task3ABB, _Team3ABB, _Technology3ABB1, _Technology3ABB2),
		column(4, '',
		       _Task4ABA, _Team4ABA, _Technology4ABA1, _Technology4ABA2,
		       _Task4ABB, _Team4ABB, _Technology4ABB1, _Technology4ABB2) ]).

/* Discussion starting 2nd week, no techniques in weeks 1 and 4 - removed after skype of 23/12/2020
boardPattern( [ column(1, '',
		       _Task1ABA, _Team1ABA, _Technology1ABA1, _Technology1ABA2,
		       _Task1ABB, _Team1ABB, _Technology1ABB1, _Technology1ABB2),
		column(2, '014',
		       '103', '201', '307', '',
		       '113', '206', or('310', '305'), ''),
		column(3, '015', % towards assignment
		       '107', or('201', '202', '203'), '311', or('310','301'),
		       '', '', '', ''),
		column(4, '',
		       _Task4ABA, _Team4ABA, _Technology4ABA1, _Technology4ABA2,
		       _Task4ABB, _Team4ABB, _Technology4ABB1, _Technology4ABB2) ]).
boardPattern( [ column(1, '',
		       _Task1ABA, _Team1ABA, _Technology1ABA1, _Technology1ABA2,
		       _Task1ABB, _Team1ABB, _Technology1ABB1, _Technology1ABB2),
		column(2, '014',
		       '103', '201', '307', '',
		       '113', '206', or('310', '305'), ''),
		column(3, '016', % towards artefact
		       '112', or('201', '202', '203'), '311', or('310','301'),
		       '', '', '', ''),
		column(4, '',
		       _Task4ABA, _Team4ABA, _Technology4ABA1, _Technology4ABA2,
		       _Task4ABB, _Team4ABB, _Technology4ABB1, _Technology4ABB2) ]).
boardPattern( [ column(1, '',
		       _Task1ABA, _Team1ABA, _Technology1ABA1, _Technology1ABA2,
		       _Task1ABB, _Team1ABB, _Technology1ABB1, _Technology1ABB2),
		column(2, '014',
		       '103', '201', '307', '',
		       '113', '206', or('310', '305'), ''),
		column(3, '017', % towards report
		       '101', or('201', '202', '203'), '308', or('310','301'),
		       '', '', '', ''),
		column(4, '',
		       _Task4ABA, _Team4ABA, _Technology4ABA1, _Technology4ABA2,
		       _Task4ABB, _Team4ABB, _Technology4ABB1, _Technology4ABB2) ]).
*/
/* Discussion starting 3rd week, no techniques in week 1 and 2 - removed after skype of 23/12/2020
boardPattern( [ column(1, '',
		       _Task1ABA, _Team1ABA, _Technology1ABA1, _Technology1ABA2,
		       _Task1ABB, _Team1ABB, _Technology1ABB1, _Technology1ABB2),
		column(2, '',
		       _Task2ABA, _Team2ABA, _Technology2ABA1, _Technology2ABA2,
		       _Task2ABB, _Team2ABB, _Technology2ABB1, _Technology2ABB2),
		column(3, '014',
		       '103', '201', '307', '',
		       '113', '206', or('310', '305'), ''),
		column(4, '015', % towards assignment
		       '107', or('201', '202', '203'), '311', or('310','301'),
		       '', '', '', '') ]).
boardPattern( [ column(1, '',
		       _Task1ABA, _Team1ABA, _Technology1ABA1, _Technology1ABA2,
		       _Task1ABB, _Team1ABB, _Technology1ABB1, _Technology1ABB2),
		column(2, '',
		       _Task2ABA, _Team2ABA, _Technology2ABA1, _Technology2ABA2,
		       _Task2ABB, _Team2ABB, _Technology2ABB1, _Technology2ABB2),
		column(3, '014',
		       '103', '201', '307', '',
		       '113', '206', or('310', '305'), ''),
		column(4, '016', % towards artefact
		       '112', or('201', '202', '203'), '311', or('310','301'),
		       '', '', '', '') ]).
boardPattern( [ column(1, '',
		       _Task1ABA, _Team1ABA, _Technology1ABA1, _Technology1ABA2,
		       _Task1ABB, _Team1ABB, _Technology1ABB1, _Technology1ABB2),
		column(2, '',
		       _Task2ABA, _Team2ABA, _Technology2ABA1, _Technology2ABA2,
		       _Task2ABB, _Team2ABB, _Technology2ABB1, _Technology2ABB2),
		column(3, '014',
		       '103', '201', '307', '',
		       '113', '206', or('310', '305'), ''),
		column(4, '017', % towards report
		       '101', or('201', '202', '203'), '308', or('310','301'),
		       '', '', '', '') ]).
*/
/* Role Play starting 1st week, no techniques in weeks 3 and 4 */
boardPattern( [ column(1, '018',
		       '111', '203', or('305', '310'), '',
		       '102', '201', '306', ''),
		column(2, '019',
		       '101', '203', '308', or('301', '310'),
		       '108', '206', or('305', '309'), ''),
		column(3, '',
		       _Task3ABA, _Team3ABA, _Technology3ABA1, _Technology3ABA2,
		       _Task3ABB, _Team3ABB, _Technology3ABB1, _Technology3ABB2),
		column(4, '',
		       _Task4ABA, _Team4ABA, _Technology4ABA1, _Technology4ABA2,
		       _Task4ABB, _Team4ABB, _Technology4ABB1, _Technology4ABB2) ]).
/* Role Play starting 2nd week - removed after skype of 23/12/2020
boardPattern( [ column(1, '',
		       _Task1ABA, _Team1ABA, _Technology1ABA1, _Technology1ABA2,
		       _Task1ABB, _Team1ABB, _Technology1ABB1, _Technology1ABB2),
		column(2, '018',
		       '111', '203', or('305', '310'), '',
		       '102', '201', '306', ''),
		column(3, '019',
		       '101', '203', '308', or('301', '310'),
		       '108', '206', or('305', '309'), ''),
		column(4, '',
		       _Task4ABA, _Team4ABA, _Technology4ABA1, _Technology4ABA2,
		       _Task4ABB, _Team4ABB, _Technology4ABB1, _Technology4ABB2) ]).
*/
/* Role Play starting 3rd week, no techniques in week 1 and 2 - removed after skype of 23/12/2020
boardPattern( [ column(1, '',
		       _Task1ABA, _Team1ABA, _Technology1ABA1, _Technology1ABA2,
		       _Task1ABB, _Team1ABB, _Technology1ABB1, _Technology1ABB2),
		column(2, '',
		       _Task2ABA, _Team2ABA, _Technology2ABA1, _Technology2ABA2,
		       _Task2ABB, _Team2ABB, _Technology2ABB1, _Technology2ABB2),
		column(3, '018',
		       '111', '203', or('305', '310'), '',
		       '102', '201', '306', ''),
		column(4, '019',
		       '101', '203', '308', or('301', '310'),
		       '108', '206', or('305', '309'), '') ]).
*/

/******************************************************************************/

/*
  expandDisjunctions(+ListWithDisjuntions, -ListOfLists)

  Disjunctions are represented as 'or'/N terms, with N in {2, 3}.
  1st arg is a list of eight Ts, some may be in the form of a disjubction.

  Beware! Only one level of nesting is supported. That is, arguments
  of the 'or' functor must be atoms, cannot be compound terms.
  Moreover, the number of arguments of the 'or' functor can only be 2 or 3.
  An error is raised otherwise.

  Usage examples:

  ?- boardPatterns:expandDisjunctions([a, or(b,c), d], Ls).
  Ls = [[a, b, d], [a, c, d]].

  ?- boardPatterns:expandDisjunctions([a, or(b,c,d), e], Ls).
  Ls = [[a, b, e], [a, c, e], [a, d, e]].

  ?- boardPatterns:expandDisjunctions([a, or(b,c,d), or(e,f)], Ls).
  Ls = [[a, b, e], [a, b, f], [a, c, e], [a, c, f], [a, d, e], [a, d, f]].

  ?- boardPatterns:expandDisjunctions([a, or(b,c,d,e), f], Ls).
  Error in expandDisjunctions/3. Input args are:
  [a]
  [or(b,c,d,e),f]
  true.

  ?- boardPatterns:expandDisjunctions([a, or(e,or(f,g))], Ls).
  Error in expandDisjunctions/3. Input args are:
  [a]
  [or(e,or(f,g))]
  true.


*/
expandDisjunctions(ListWithDisjuntions, ListOfLists) :-
	expandDisjunctions([], ListWithDisjuntions, ListOfLists).

expandDisjunctions(Prefix, [], [Prefix]) :- !.
expandDisjunctions(Prefix, [Element | Suffix], Results) :-
	var(Element),
	!,
	append(Prefix, [Element], NewPrefix),
	expandDisjunctions(NewPrefix, Suffix, Results).
expandDisjunctions(Prefix, [Element | Suffix], Results) :-
	atom(Element),
	!,
	append(Prefix, [Element], NewPrefix),
	expandDisjunctions(NewPrefix, Suffix, Results).
expandDisjunctions(Prefix, [or(Arg1, Arg2) | Suffix], Results) :-
	atom(Arg1),
	atom(Arg2),
	!,
	append(Prefix, [Arg1 | Suffix], L1),
	expandDisjunctions(L1, R1),
	append(Prefix, [Arg2 | Suffix], L2),
	expandDisjunctions(L2, R2),
	append(R1, R2, Results).
expandDisjunctions(Prefix, [or(Arg1, Arg2, Arg3) | Suffix], Results) :-
	atom(Arg1),
	atom(Arg2),
	atom(Arg3),
	!,
	append(Prefix, [Arg1 | Suffix], L1),
	expandDisjunctions(L1, R1),
	append(Prefix, [Arg2 | Suffix], L2),
	expandDisjunctions(L2, R2),
	append(Prefix, [Arg3 | Suffix], L3),
	expandDisjunctions(L3, R3),
	append(R1, R2, Temp),
	append(Temp, R3, Results).
expandDisjunctions(X, Y, _) :-
	debug_format('Error in expandDisjunctions/3. Input args are:~n~k~n~k~n', [X, Y]).

/*
  cartesianProduct(+List1, +List2, +List3, +List4, -ResultList)

  ResultList elements are lists of four elements_ all possible permutations of elements of input lists

  Usage examples:

  ?- boardPatterns:cartesianProduct([a, b], [c, d], [e, f], [g, h], L).
  L = [[a, c, e, g], [a, c, e, h], [a, c, f, g], [a, c, f, h], [a, d, e, g], [a, d, e|...], [a, d|...], [a|...], [...|...]|...].

  ?- boardPatterns:cartesianProduct([a], [c], [e, f], [g, h], L).
  L = [[a, c, e, g], [a, c, e, h], [a, c, f, g], [a, c, f, h]].


*/
cartesianProduct(A, B, C, D, Result) :-
    findall([X, Y, Z, W],(member(X, A), member(Y, B), member(Z, C), member(W, D)),Result).


expandFourColumns([Col1, Col2, Col3, Col4], Result) :-
	expandColumn(Col1, EL1),
	expandColumn(Col2, EL2),
	expandColumn(Col3, EL3),
	expandColumn(Col4, EL4),
	cartesianProduct(EL1, EL2, EL3, EL4, Result).

/*
  buildColumns(+ColNumber, +Technique, +LstOfEightTs, -ListOfColumns).

  ListOfColumns contains terms in the form:
    column(ColNumber, Technique,
           TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
           TaskABB, TeamABB, TechnologyABB1, TechnologyABB2).

  */
buildColumns(_ColNumber, _Technique, [], []).
buildColumns(ColNumber, Technique, [EightTs | Rest], [Column | Columns]) :-
	Column =.. [column, ColNumber, Technique | EightTs],
	buildColumns(ColNumber, Technique, Rest, Columns).


expandColumn(column(ColNumber, Technique,
		    TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		    TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
	     [column(ColNumber, Technique,
		     TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		     TaskABB, TeamABB, TechnologyABB1, TechnologyABB2)]) :-
	var(Technique),
	debug_format('expandColumn with unbound Technique - should never happen~n', []).
expandColumn(column(ColNumber, Technique,
		    TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		    TaskABB, TeamABB, TechnologyABB1, TechnologyABB2),
	     ListOfColumns) :-
	nonvar(Technique),
	/*
	  technique(Technique, TechniqueName, _, Phase, _),
	  debug_format('expandColumn ~d of technique ~w phase ~w~n', [ColNumber, TechniqueName, Phase]),
	*/
	expandDisjunctions([TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
			    TaskABB, TeamABB, TechnologyABB1, TechnologyABB2],
			   ListOfEightTs),
	swapTechnologies(ListOfEightTs, Swapped),
	/*
	  length(ListOfEightTs, Len1),
	  length(Swapped, Len2),
	  debug_format('~d --> ~d~n', [Len1, Len2]),
	  */
	buildColumns(ColNumber, Technique, Swapped, ListOfColumns).


/*
  swapTechnologies(+ListOfEightTs, -ListOfEightTsWithThechnlogiesSwapped)


  ListOfEightTsWithThechnlogiesSwapped contains at most four elements for each element in ListOfEightTs:
    [TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
     TaskABB, TeamABB, TechnologyABB1, TechnologyABB2],
    [TaskABA, TeamABA, TechnologyABA2, TechnologyABA1,
     TaskABB, TeamABB, TechnologyABB1, TechnologyABB2],
    [TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
     TaskABB, TeamABB, TechnologyABB2, TechnologyABB1],
    [TaskABA, TeamABA, TechnologyABA2, TechnologyABA1,
     TaskABB, TeamABB, TechnologyABB2, TechnologyABB1]
  However, duplicates are removed, so there might be less than four elements in the result

  If both technologies are unbound the column is unspecified in that technique, so non swap occurs: 1 --> 1

  i.e., technology cards are swapped in four possible ways:
     ABA1, ABA2, ABB1, ABB2
     ABA2, ABA1, ABB1, ABB2
     ABA1, ABA2, ABB2, ABB1
     ABA2, ABA1, ABB2, ABB1
  as far as nonvar ABA1 \= ABA2, ABB1 \= ABB2 etc.

*/
swapTechnologies([], []).
swapTechnologies([[TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		   TaskABB, TeamABB, TechnologyABB1, TechnologyABB2] | RestOfEightTs],
		 [[TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		   TaskABB, TeamABB, TechnologyABB1, TechnologyABB2] | RestOfEightTsWithThechnlogiesSwapped]) :-
	(var(TechnologyABA1) ; var(TechnologyABA2) ; var(TechnologyABB1) ; var(TechnologyABB2)),
	!,
	% debug_format('At least one technology slot is unbound~n', []),
	swapTechnologies(RestOfEightTs, RestOfEightTsWithThechnlogiesSwapped).
swapTechnologies([[TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
		   TaskABB, TeamABB, TechnologyABB1, TechnologyABB2] | RestOfEightTs],
		 Result) :-
	/* sort removes duplicates */
	sort([[TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
	       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2],
	      [TaskABA, TeamABA, TechnologyABA2, TechnologyABA1,
	       TaskABB, TeamABB, TechnologyABB1, TechnologyABB2],
	      [TaskABA, TeamABA, TechnologyABA1, TechnologyABA2,
	       TaskABB, TeamABB, TechnologyABB2, TechnologyABB1],
	      [TaskABA, TeamABA, TechnologyABA2, TechnologyABA1,
	       TaskABB, TeamABB, TechnologyABB2, TechnologyABB1]],
	     Sorted),
	swapTechnologies(RestOfEightTs, RestOfEightTsWithThechnlogiesSwapped),
	append(Sorted, RestOfEightTsWithThechnlogiesSwapped, Result). % <-- da ottimizzare




/* INIT CODE */

/* To assert all the boardPatternExpanded/1 clauses: */
assertAllPatterns() :-
	open('boardPatternExpanded.pl', write, Stream, [close_on_abort(true)]),
	retractall(kbBase:boardPatternExpanded(_)),
	doAssertAllPatterns(Stream),
	close(Stream).

doAssertAllPatterns(Stream) :-
	boardPattern(FourColumns),
	expandFourColumns(FourColumns, ListOfFourColumns),
	forall(member(X, ListOfFourColumns),
		   (   Term =.. [boardPatternExpanded, X],
			   assert(Term),
			   write_term(Stream, Term, [max_depth(0), quoted(true), fullstop(true), nl(true)])
		   )
		  ),
	fail.
doAssertAllPatterns(_Stream).

:- assertAllPatterns.
