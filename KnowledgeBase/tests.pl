% -*- Mode: Prolog -*-

:- module(tests, []).

:- use_module(kbCommons).
:- use_module(kbBase).
:- use_module(kbAdvanced).

:- use_module(httpDriver, [semanticCheck/2, buildBoard/2]).

run() :-
        directory_files('tests/httpTests', ['.','..' | HTTPfiles]),
        httpValidation_tests(HTTPfiles, HTTPexecuted, HTTPpassed, HTTPfailed),
        directory_files('tests', ['.','..' | AllFiles]),
        boardValidation_tests(AllFiles, Executed, Passed, Failed),
        TotalExecuted is HTTPexecuted + Executed,
        TotalPassed is HTTPpassed + Passed,
        TotalFailed is HTTPfailed + Failed,
        debug_format('Testing summary~n  Executed: ~w~n  Passed: ~w~n  Failed: ~w~n', [TotalExecuted, TotalPassed, TotalFailed]).

boardValidation_tests([], 0, 0, 0) :- !.
boardValidation_tests([FileName | OtherFiles], Executed, Passed, Failed) :-
        concat('tests/', FileName, InputFilePath),
        (exists_file(InputFilePath) -> true; debug_format('skipping ~w - not a file~n', [InputFilePath]), fail),
        debug_format('processing ~w~n', [InputFilePath]),
        open(InputFilePath, read, StreamInput, [close_on_abort(true)]),
        read_term(StreamInput, BoardSetting, []),
        close(StreamInput),
%        debug_format('input is ~w~n', [BoardSetting]),
        getTestID(FileName, TestID),
        concat('tests/results/', TestID, ResultPath),
        open(ResultPath, read, Stream, [close_on_abort(true)]),
        read_term(Stream, Result, []),
        close(Stream),
%        debug_format('result is ~w~n', [Result]),!,
        Result =.. [_,Inconsistencies, InconsistencyDetails|_], length(Inconsistencies, NumOfInconsistencies), length(InconsistencyDetails, NumOfInconsistencyDetails),
        (NumOfInconsistencies = NumOfInconsistencyDetails ->
                (semanticCheck(BoardSetting, ActualResult) -> 
                        (ActualResult = Result -> /*debug_format('PASSED~n', []),*/
                                PassedThis = 1, FailedThis = 0;
                                debug_format('FAILED for ~w: expected ~w, actual ~w~n', [InputFilePath, Result, ActualResult]), PassedThis = 0, FailedThis = 1
                        );
                        debug_format('FAILED for ~w: semantic check failed to evaluate~n', [InputFilePath]), PassedThis = 0, FailedThis = 1
                );
                debug_format('FAILED for ~w: number of inconsistencies and their details do not match [~w != ~w]~n', [InputFilePath, NumOfInconsistencies, NumOfInconsistencyDetails]), PassedThis = 0, FailedThis = 1
        ),
        boardValidation_tests(OtherFiles, ExecutedRest, PassedRest, FailedRest), !,
        Executed is ExecutedRest + 1,
        Passed is PassedRest + PassedThis,
        Failed is FailedRest + FailedThis.
boardValidation_tests([_|OtherFiles], Executed, Passed, Failed) :-
        boardValidation_tests(OtherFiles, Executed, Passed, Failed).

httpValidation_tests([], 0, 0, 0) :- !.
httpValidation_tests([FileName | OtherFiles], Executed, Passed, Failed) :-
        concat('tests/httpTests/', FileName, InputFilePath),
        (exists_file(InputFilePath) -> true; debug_format('skipping ~w - not a file~n', [InputFilePath]), fail),
        debug_format('processing ~w~n', [InputFilePath]),
        open(InputFilePath, read, StreamInput, [close_on_abort(true)]),
        read_term(StreamInput, HTTPrequest, []),
        close(StreamInput),
%        debug_format('input is ~w~n', [BoardSetting]),
        getTestID(FileName, TestID),
        concat('tests/httpResults/', TestID, ResultPath),
        open(ResultPath, read, Stream, [close_on_abort(true)]),
        read_term(Stream, Result, []),
        close(Stream),
%        debug_format('result is ~w~n', [Result]),!,
        (buildBoard(HTTPrequest, Board), semanticCheck(Board, ActualResult) -> 
                (ActualResult = Result -> /*debug_format('PASSED~n', []),*/ PassedThis = 1, FailedThis = 0; debug_format('FAILED for ~w: expected ~w, actual ~w~n', [InputFilePath, Result, ActualResult]), PassedThis = 0, FailedThis = 1);
                debug_format('request for ~w FAILED~n', [InputFilePath]), PassedThis = 0, FailedThis = 1
        ),
        httpValidation_tests(OtherFiles, ExecutedRest, PassedRest, FailedRest), !,
        Executed is ExecutedRest + 1,
        Passed is PassedRest + PassedThis,
        Failed is FailedRest + FailedThis.
httpValidation_tests([_|OtherFiles], Executed, Passed, Failed) :-
        httpValidation_tests(OtherFiles, Executed, Passed, Failed).

getTestID(Name, ID) :-
        atom_chars(Name, NameList),
        append([_|_], ['_'|IDList], NameList),!,
        atom_chars(ID, IDList).