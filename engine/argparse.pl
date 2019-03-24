:- module(argparse, []).

:- use_module(library(debug)).
:- use_module(library(settings)).

:- setting(rules, atom,   '../data/private/rules.pl', 'The location of the rules file').
:- setting(ontology, atom,   '../data/private/ontology.ttl', 'The location of the ontology file').
:- setting(port, atom,   '3001', 'The port of the server').
:- setting(ip_address, atom,   'localhost', 'The ip address of the server').
:- setting(user_name, atom,   'janzi', 'User name to log into server').
:- setting(user_code, atom,   'lRkCA2Gaq83mlGxgkpgy', 'User codes to log into server').


arg(Name, Value) :- setting(Name, Value).
 

args :-
    load_settings('../data/engine_settings.pl'),
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

args(['--user', Value | Tail ]) :-
    debug(info, 'Using user \'~w\'.', [Value]),
    set_setting(user_name, Value),
    args(Tail).
args(['-u', Value | Tail ]) :-
    debug(info, 'Using user \'~w\'.', [Value]),
    set_setting(user_name, Value),
    args(Tail).

args(['--code', Value | Tail ]) :-
    debug(info, 'Using code \'~w\'.', [Value]),
    set_setting(user_code, Value),
    args(Tail).
args(['-c', Value | Tail ]) :-
    debug(info, 'Using code \'~w\'.', [Value]),
    set_setting(user_code, Value),
    args(Tail).


