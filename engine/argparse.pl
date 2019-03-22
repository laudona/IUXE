:- module(argparse, []).

:- use_module(library(debug)).
:- use_module(library(settings)).

:- setting(rules, atom,   '../data/private/rules.pl', 'The location of the rules file').
:- setting(ontology, atom,   '../data/private/ontology.ttl', 'The location of the ontology file').
:- setting(port, atom,   '3001', 'The port of the server').
:- setting(ip_address, atom,   'localhost', 'The ip address of the server').

args :-
    current_prolog_flag(argv, Argv),
    args(Argv).

args([]).
args([_]).

args(['--rules', RulesFile | Tail ]) :-
    debug(info, 'Using rule \'~w\'.', [RulesFile]),
    set_setting(rules, RulesFile),
    args(Tail).
args(['-r', RulesFile | Tail ]) :-
    debug(info, 'Using rule \'~w\'.', [RulesFile]),
    set_setting(rules, RulesFile),
    args(Tail).

args(['--ontology', OntologyFile | Tail ]) :-
    debug(info, 'Using ontology \'~w\'.', [OntologyFile]),
    set_setting(ontology, OntologyFile),
    args(Tail).
args(['-o', OntologyFile | Tail ]) :-
    debug(info, 'Using ontology \'~w\'.', [OntologyFile]),
    set_setting(ontology, OntologyFile),
    args(Tail).

args(['--port', Port | Tail ]) :-
    debug(info, 'Using port \'~w\'.', [Port]),
    set_setting(port, Port),
    args(Tail).
args(['-p', Port | Tail ]) :-
    debug(info, 'Using port \'~w\'.', [Port]),
    set_setting(port, Port),
    args(Tail).

args(['--ip-address', IpAddress | Tail ]) :-
    debug(info, 'Using ip-address \'~w\'.', [IpAddress]),
    set_setting(ip_address, IpAddress),
    args(Tail).
args(['-i', IpAddress | Tail ]) :-
    debug(info, 'Using ip-address \'~w\'.', [IpAddress]),
    set_setting(ip_address, IpAddress),
    args(Tail).
