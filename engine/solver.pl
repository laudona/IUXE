:- module(solver, []).

:- use_module(library(debug)).
:- use_module(library(lists)).

:- use_module(rule_reader, []).
:- use_module(rule_transformer, []).
:- use_module(ontology_reader, []).


solve(true, true).
solve(and(Left, Right), and(LeftExplanation, RightExplanation)) :-
    solve(Left, LeftExplanation),
    solve(Right, RightExplanation).
solve(or(Left, Right), or(Explanation)) :-
    solve(Left, Explanation);
    solve(Right, Explanation).
solve(not(Goal), not(Goal)) :-
    \+ solve(Goal, _Expl).
solve(expression(Term), expression(Term)) :-
    call(Term).
solve(goal(Type, S, P, O), believe) :-
    Type \= action,
    Type \= intermediate,
    Type \= unbelieve,
    debug(debug/solver/solve, '[SOLVER] considering fact goal ~w(\'~w\' - \'~w\' - \'~w\').', [Type, S, P, O]),
    ontology_reader:read_rdf(Type, S, P, O).
solve(goal(Type, S, P, O), rule(Name, Explanation)) :-
    Type \= event,
    rule_reader:rule(Name, Conditions, goal(Type, S, P, O)),
    debug(debug/solver/solve, '[SOLVER] considering rule goal ~w(\'~w\' - \'~w\' - \'~w\').', [Type, S, P, O]),
    debug(debug/solver/solve, '[SOLVER] required conditions are :[ ~w ].', [Conditions]),
    solve(Conditions, Explanation).
solve(CompoundTerm, Explanation) :-
    CompoundTerm =.. [Functor, S, P, O],
    Functor \= and,
    Functor \= or,
    Functor \= not,
    Functor \= true,
    Functor \= goal,
    Functor \= expression,
    debug(debug/solver/solve, '[SOLVER] pack type ~w in goal format.', [Functor]),
    solve(goal(Functor, S, P, O), Explanation).
solve(condition(believe, S, P, O), believe) :-
    debug(debug/solver/solve, '[SOLVER] considering believe(\'~w\' - \'~w\' - \'~w\').', [S, P, O]),
    ontology_reader:read_rdf(believe, S, P, O).
solve(condition(Type, S, P, O), Explanation) :-
    Type \= action,
    Type \= unbelieve,
    Type \= believe,
    debug(debug/solver/solve, '[SOLVER] convert type ~w in from condition to goal.', [Type]),
    solve(goal(Type, S, P, O), Explanation).


solve_unbelieves(Unbelieves) :-
    findall(unbelieve(S, P, O, Explanation) ,solve(goal(unbelieve, S, P, O), Explanation), Unbelieves).

solve_believes(Believes) :-
    findall(believe(S, P, O, Explanation) ,solve(goal(believe, S, P, O), Explanation), Believes).

solve_actions(Actions) :-
    findall(action(S, P, O, Explanation) ,solve(goal(action, S, P, O), Explanation), Actions).

solve_event(Event, EventData, _EventDataType, Response) :-
    debug(info/solver/solve_event, '[SOLVER] Running solver with event ~w .', [Event]),
    debug(debug/solver/solve_event, '[SOLVER] Running solver with event data: ~w', [EventData]),
    replace_events(string(EventData)),
    run(Response), !,
    debug(info/solver/solve_event, 'Actions concluded: ~w', [Response]).
solve_event(Event, _EventData, _EventDataType, []) :-
    debug(warn/solver/solve_event, '[SOLVER][WARN] solver failed for event ~w, no conclusions submitted.', [Event]).


map_uri(Uri, Root) :-
    atom_length('http://www.tudelft.nl/ewi/iuxe#', Start),
    sub_atom(Uri, Start, _Len, 0, Root).

build_turtle(goal(action, S, P, O, _Expl), Turtle) :-
    atom(O), !,
    format(atom(Turtle), '<~w> <~w> <~w> .', [S, P, O]).
build_turtle(goal(action, S, P, ^^(Value, Type)), Turtle) :-
    !,
    format(atom(Turtle), '<~w> <~w> "~w"^^<~w> .', [S, P, Value, Type]).
build_turtle(goal(action, S, P, O), Turtle) :-
    string(O), !,
    format(atom(Turtle), '<~w> <~w> "~w" .', [S, P, O]).
build_turtle(goal(action, S, P, O), Turtle) :-
    integer(O), !,
    format(atom(Turtle), '<~w> <~w> "~w"^^<http://www.w3.org/2001/XMLSchema#integer> .', [S, P, O]).
build_turtle(goal(action, S, P, O), Turtle) :-
    float(O), !,
    format(atom(Turtle), '<~w> <~w> "~w"^^<http://www.w3.org/2001/XMLSchema#decimal> .', [S, P, O]).
build_turtle(goal(action, S, P, O), Turtle) :-
    debug(solver/info, '[WARNING] unknown object type for ~w', [O]),
    format(atom(Turtle), '<~w> <~w> <~w> .', [S, P, O]).

%%
%% Loads the engine data in a specific directory. It looks for the following data files:
%%      - rules.pl - the rules file.
%%      - ontology.ttl - the initial believes
%%      - events.ttl - the events used for an initial run.
%%
solve_directory(Dir, ResponseActions) :-
    rule_transformer:initialize_prefixes,
    debug(debug/solve/solve_directory, 'Removing all data.', []),
    reset_everything,
    absolute_file_name(Dir, Absolute),
    exists_directory(Absolute),
    debug(debug/solve/solve_directory, 'Loading directory "~w".', [Absolute]),
    setup_call_cleanup(
        working_directory(Old, Absolute),
        solve_files('./rules.pl', './ontology.ttl', './events.ttl', ResponseActions),
        working_directory(_WhatEver, Old) 
    ).
solve_directory(Dir, []) :-
    \+ exists_directory(Dir),
    debug(warn/solve/solve_directory, 'Could not find directory "~w", aborting solver!', [Dir]).
 solve_directory(Dir, []) :-
    debug(warn/solve/solve_directory, 'Solve directory "~w" failed due to unknown reasons.', [Dir]).   


%%
%% Loads the specified files and runs the reasoner on the data.
%%
solve_files(RulesFile, OntologyFile, EventFile, Actions) :-
    load_rules(RulesFile),
    load_ontology(OntologyFile),
    load_events(EventFile),
    run(Actions).
solve_files(_RulesFile, _OntologyFile, _EventFile, []) :-
    debug(warn/solve/solve_files, 'Solve files failed, no actions resulted.', []).

%%
%% Runs a single pass of the reasoner on the exiting data.
%%
run(Response) :-
    debug(info/solver/run, 'Solver run .', []),
    solve_unbelieves(Unbelieves), !,
    solve_believes(Believes), !,
    solve_actions(Actions), !,
    handle_unbelieves(Unbelieves), !,
    handle_believes(Believes), !,
    handle_actions(Actions), !,
    build_response(Actions, Response), !.
run([]) :-
    debug(warn/solve/run, 'Solver run failed, no conclusions drawn!', []).    

%%
%% Handle unbelieve conculsions.
%%
handle_unbelieves([]).
handle_unbelieves([ Item | List]) :-
    handle_unbelieve(Item),
    handle_unbelieves(List).

%%
%% Handle an unbelieve conclusion.
%%
handle_unbelieve(unbelieve(S, P, O, _Expl)) :-
    \+ ontology_reader:read_rdf(believe, S, P, O), !,
    debug(debug/solver/handle_unbelieve, 'Skipping concluded unbelieve <~w> - <~w> - "~w", because it was not in the believes.', [S, P, O]).
handle_unbelieve(unbelieve( S, P, O, _Expl)) :-
    ontology_reader:delete_rdf(believe, S, P, O), !,
    debug(info/solver/handle_unbelieve, 'Retracting believe <~w> - <~w> - "~w" because it was unbelieved.', [S, P, O]).
handle_unbelieve(Unbelieve) :-
    !, debug(warn/solver/handle_unbelieve, 'Unable to handle unbelieve "<~w>" because it was an unknown format.', [Unbelieve]).

%%
%% Handle believe conculsions.
%%
handle_believes([]).
handle_believes([ Item | List]) :-
    handle_believe(Item),
    handle_believes(List).

%%
%% Handle a believe conclusion.
%%
handle_believe(believe(S, P, O, Expl)) :-
    Expl = believe, !,
    debug(debug/solver/handle_believe, 'Skipping concluded believe <~w> - <~w> - "~w", because it was derived from the believes set.', [S, P, O]).
handle_believe(believe(S, P, O, Expl)) :-
    Expl \= believe, !,
    ontology_reader:write_rdf(believe, S, P, O),
    debug(debug/solver/handle_believe, 'Writing believe <~w> - <~w> - "~w" because it was concluded.', [S, P, O]).
handle_believe(believe(S, P, O, Expl)) :-
    !, debug(warn/solver/handle_believe, 'Why did "~w" fail? Should be either beleive or rule!', [believe(S, P, O, Expl)]).
handle_believe(believe(S, P, O)) :-
    !, debug(warn/solver/handle_believe, '[WARN] Old style believe without explanation! "~w".', [believe(S, P, O)]).
handle_believe(Believe) :-
    !, debug(warn/solver/handle_believe, 'Unable to handle believe "~w" because it was an unknown format.', [Believe]).

%%
%% Handle actions.
%%
handle_actions([]).
handle_actions([ Item | List]) :-
    handle_action(Item),
    handle_actions(List).

%%
%% Handle action that should affect rules engine.
%%
handle_action(action(_S, _P, _O, _Expl)) :- 
    !, true.  %% Option to handle actions on the engine...
handle_action(Item) :-
    !, debug(warn/solver/handle_action, 'Unable to handle action "~w" because it was an unknown format.', [Item]).


%%
%% Build a response based on the concluded actions.
%%
build_response([], []).
build_response([ Action | Actions ], [ RespAction | Response ]) :-
    build_response_action(Action, RespAction),
    build_response(Actions, Response).

%%
%% convert action to response format.
%%
build_response_action(Action, none) :-
    Action \= action(_S, _P, _O, _Expl), !,
    debug(warn/solver/build_response_action, 'Unknown action format "<~w>" ignored.', [Action]).
build_response_action(action(S, P, O, _Expl), RespAction) :-
    map_uri(S, SubjectRoot),
    map_uri(P, PredicateRoot),
    build_turtle(goal(action, S, P, O), Turtle),
    format(atom(Name), '~w.action.~w', [SubjectRoot, PredicateRoot]),
    RespAction = action(Name, Turtle, 'text/turtle'), !,
    debug(info/solver/build_response_action, 'Building response ~w.', [RespAction]).
build_response_action(Action, none) :-
    debug(warn/solver/build_response_action, 'Convering action "<~w>" failed.', [Action]).

%%
%% Loads the rules from the specified file.
%%
load_rules(RulesFile) :-
    exists_file(RulesFile),
    rule_reader:load_rules_file(RulesFile),
    debug(info/solve/load_rules, 'Rules "~w" loaded!', [RulesFile]).
load_rules(RulesFile) :-
    \+ exists_file(RulesFile),
    debug(warn/solve/load_rules, 'Rules "~w" not found, continueing with empty rules set.', [RulesFile]).

%%
%% Loads the ontology from the specified file.
%%
load_ontology(OntologyFile) :-
    exists_file(OntologyFile),
    ontology_reader:load_ontology_file(believe, OntologyFile),
    debug(info/solve/load_ontology, 'Ontology "~w" loaded!', [OntologyFile]).
load_ontology(OntologyFile) :-
    \+ exists_file(OntologyFile),
    debug(warn/solve/load_ontology, 'Ontology "~w" not found, continueing with empty initial believe set.', [OntologyFile]).

%%
%% Replaces the current set of events with a new one from file or string.
%%
replace_events(string(EventsString)) :-
    reset_events,
    load_events_from_string(EventsString).
replace_events(file(EventsFile)) :-
    reset_events,
    load_events(EventsFile).

%%
%% Loads the initial events from the specified file.
%%
load_events(EventsFile) :-
    exists_file(EventsFile),
    ontology_reader:load_ontology_file(event, EventsFile),
    debug(info/solve/load_events, 'Events "~w" loaded!', [EventsFile]).
load_events(EventsFile) :-
    \+ exists_file(EventsFile),
    debug(warn/solve/load_events, 'Events "~w" not found, continueing with empty initial event set.', [EventsFile]).

%%
%% Loads a set of events from the string.
%%
load_events_from_string(EventsString) :-
    ontology_reader:load_ontology_string(event, EventsString),
    debug(debug/solve/load_events, 'Events "~w" loaded!', [EventsString]).
load_events_from_string(EventsString) :-
    debug(warn/solve/load_events, 'Failed to load events ~w, wrong format maybe?', [EventsString]).

%%
%% Remove all known rules, believes and events.
%%
reset_everything :- 
    reset_believes,
    reset_events,
    reset_all_rules.
reset_events :- ontology_reader:reset_graph(event).
reset_believes :- ontology_reader:reset_graph(believe).
reset_all_rules :- rule_reader:reset_rules.

%%
%% List the curretn believes
%%
list_believes :-
    findall(believe(S, P, O), ontology_reader:read_believe(S, P, O), Bs),
    list_believes(Bs).

list_believes([]).
list_believes([ B | Rest ]) :-
    debug(info/solver/list_believes, '~w', B),
    list_believes(Rest).

%%
%% Unit tests
%%
:- begin_tests(solver).

test(load_directory_basic, [nondet,  setup(reset_everything), cleanup(reset_everything)]) :-
    debug(warn/_Mod/_Pred1),
    debug(info/solve/_Pred2),
    debug(debug/solve/solve_directory),
    nodebug(debug/ontology_reader/load_ontology_file),
    solve_directory('test/data/basic', _Actions).

test(run_basic_rule_exists, [nondet,  setup(reset_everything), cleanup(reset_everything),
    true(Name = basic_rule_1)]) :-
    debug(warn/_Mod/_Pred1),
    debug(info/solve/_Pred2),
    debug(debug/solve/solve_directory),
    nodebug(debug/ontology_reader/load_ontology_file),
    solve_directory('test/data/basic', _Actions),
    rule_reader:find_rule(Name, _Cond, goal(action, 'http://www.tudelft.nl/ewi/iuxe#user', 'http://www.tudelft.nl/ewi/iuxe#do', _ExObject)).

test(run_basic_action_concluded, [nondet,  setup(reset_everything), cleanup(reset_everything),
    true(
        member( action('user.action.do', _A, _B), Actions )
    )
    ]) :-
    debug(warn/_Mod/_Pred1),
    debug(info/solve/_Pred2),
    debug(debug/solve/solve_directory),
    nodebug(debug/ontology_reader/load_ontology_file),
    solve_directory('test/data/basic', Actions).

test(run_basic_action_concluded, [nondet,  setup(reset_everything), cleanup(reset_everything),
    true(
        Turtle =  '<http://www.tudelft.nl/ewi/iuxe#user> <http://www.tudelft.nl/ewi/iuxe#play> "Son Of A Preacher Man" .'
    )
    ]) :-
    debug(warn/_Mod/_Pred1),
    debug(info/solve/_Pred2),
    debug(debug/solve/solve_directory),
    nodebug(debug/ontology_reader/load_ontology_file),
    solve_directory('test/data/basic', Actions),
    member( action('user.action.play', Turtle, _B), Actions ).

test(run_basic_chain_through_intermediate, [nondet,  setup(reset_everything), cleanup(reset_everything),
    true(
        Turtle =  '<http://www.tudelft.nl/ewi/iuxe#chain> <http://www.tudelft.nl/ewi/iuxe#intermediate> <http://www.tudelft.nl/ewi/iuxe#nothing> .'
    )
    ]) :-
    debug(warn/_Mod/_Pred1),
    debug(info/solve/_Pred2),
    debug(debug/solve/solve_directory),
    nodebug(debug/ontology_reader/load_ontology_file),
    solve_directory('test/data/basic', Actions),
    member( action('chain.action.intermediate', Turtle, _B), Actions ).

test(run_basic_believe_loaded, [nondet,  setup(reset_everything), cleanup(reset_everything),
    true(
        O = 9000        
    )
    ]) :-
    debug(warn/_Mod/_Pred1),
    debug(info/solve/_Pred2),
    debug(debug/solve/solve_directory),
    nodebug(debug/ontology_reader/load_ontology_file),
    solve_directory('test/data/basic', _Actions),
    ontology_reader:read_believe('http://www.tudelft.nl/ewi/iuxe#believe_power', 'http://www.tudelft.nl/ewi/iuxe#over', O).

test(run_basic_believe_on_string, [nondet,  setup(reset_everything), cleanup(reset_everything),
    true(
        Pred = 'http://www.tudelft.nl/ewi/iuxe#name'        
    )
    ]) :-
    debug(warn/_Mod/_Pred1),
    debug(info/solve/_Pred2),
    debug(debug/solve/solve_directory),
    nodebug(debug/ontology_reader/load_ontology_file),
    solve_directory('test/data/basic', _Actions),
    ontology_reader:read_believe(_Sub, Pred, "Son Of A Preacher Man").

test(run_basic_unbelieve_preview, [setup(reset_everything), cleanup(reset_everything),
    fail
    ]) :-
    debug(warn/_Mod/_Pred1),
    solve_directory('test/data/basic', _Actions), !,
    ontology_reader:read_believe(_Sub, 'http://www.tudelft.nl/ewi/iuxe#preview_url', _Object).

test(run_basic_with_second_event, [setup(reset_everything), cleanup(reset_everything),
    true(
        Turtle = '<http://www.tudelft.nl/ewi/iuxe#second_pass> <http://www.tudelft.nl/ewi/iuxe#at> "1"^^<http://www.w3.org/2001/XMLSchema#integer> .'        
    )
    ]) :-
    debug(warn/_Mod/_Pred1),
    solve_directory('test/data/basic', _Actions), !,
    Event = 'agent.event.has',
    EventData = '<http://www.tudelft.nl/ewi/iuxe#agent> <http://www.tudelft.nl/ewi/iuxe#has> <http://www.tudelft.nl/ewi/iuxe#second_pass> .',
    solve_event(Event, EventData, 'text/turtle', Response), !,
    member( action('second_pass.action.at', Turtle, _B), Response ).
     
:- end_tests(solver).