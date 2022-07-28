% -*- Mode: Prolog -*-

:- module('4T_Game', [version/2]).

:- ['httpDriver.pl',
    'kbCommons.pl',
    'kbBase.pl',
    'kbAdvanced.pl',
    'correctnessBase.pl',
    'correctnessAdvanced.pl',
    'suggestionsBase.pl',
    'suggestionsAdvanced.pl',
    'completenessBase.pl'
   ].

version('4.42', 'Mar 05, 2021').


:- server(8000).
