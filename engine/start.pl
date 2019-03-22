:- module(start, []).

:- use_module(argparse, []).
:- use_module(library(debug)).

go :-
    debug(info),
    argparse:args.
