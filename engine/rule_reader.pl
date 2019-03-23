:- module(rule_reader, []).

:- use_module(library(debug)).
:- use_module(rule_transformer, []).

:- dynamic
        rule/3.

:- op(1200,	xfy	, then).
:- op(1100,	xfy	, or).
:- op(1000, xfy , and).
:- op(900, fy , not).

%%
%% Load rules from specified file.
%%
load_rules_file(File) :-
    debug(rule_reader/load/info, 'loading rules ~w...', [File]),
    debug(rule_reader/load/info, 'removing existing rules...', []),
    retractall(rule(_Name,_Cond,_Goal)),
    debug(rule_reader/load/info, 'existing rules removed.', []),
    debug(rule_reader/load/info, 'opening ~w file...', [File]),
    open(File, read, Stream),
    debug(rule_reader/load/info, '~w file opened', [File]),
    call_cleanup(load_rules_stream(Stream), close(Stream)),
    debug(rule_reader/load/info, '~w file closed.', [File]).

%%
%% Load rules from specified stream.
%%
load_rules_stream(Stream) :-
        read(Stream, Term),
        debug(rule_reader/load/debug, 'reading term ~w...', [Term]),
        load_rules_term(Term, Stream). 

%%
%% Parse term and read next term.
%%
load_rules_term(end_of_file, _) :- !.
load_rules_term(rule(Name, Rule), Stream) :- !,
    debug(rule_reader/load/info, 'parsing rule definition \'~w\'.', [rule(Name, Rule)]),
    rule_transformer:transform(rule(Name, Rule), ExpandedRule),
    assertz(ExpandedRule),
    read(Stream, Term),
    debug(rule_reader/load/debug, 'reading term ~w...', [Term]),
    load_rules_term(Term, Stream).
load_rules_term(:-module(Name, Predicates), Stream) :- !,
    debug(rule_reader/load/info, 'parsing module definition \'~w\'.', [module(Name, Predicates)]),
    read(Stream, Term),
    debug(rule_reader/load/debug, 'reading term ~w...', [Term]),
    load_rules_term(Term, Stream).
load_rules_term(:-op(Prio,Type,Op), Stream) :- !,
    debug(rule_reader/load/info, 'parsing operator definition \'~w\'.', [op(Prio,Type,Op)]),
    call(op(Prio,Type,Op)),
    read(Stream, Term),
    debug(rule_reader/load/debug, 'reading term ~w...', [Term]),
    load_rules_term(Term, Stream).
load_rules_term(Term, _Stream) :- 
    debug(rule_reader/load/info, 'invalid term \'~w\'!', [Term]),
    type_error(rule, Term).

%%
%% Find as rule given a specific goal
%%
find_rule(Name, Cond, Goal) :- rule(Name, Cond, Goal).

%%
%% Tests
%%
:- begin_tests(rule_reader).

test(load_rules_1, []) :-
    debug(rule_reader/load/info),
    load_rules_file('../data/rules.pl').    

:- end_tests(rule_reader).