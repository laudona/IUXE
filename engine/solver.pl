:- module(solver, []).

:- use_module(library(debug)).
:- use_module(library(lists)).

:- use_module(rule_reader, []).
:- use_module(ontology_reader, []).


solve(true).
solve(and(Left, Right)) :-
    solve(Left),
    solve(Right).
solve(or(Left, Right)) :-
    solve(Left);
    solve(Right).
solve(not(Goal)) :-
    \+ solve(Goal).
solve(expression(Term)) :-
    call(Term).
solve(goal(Type, S, P, O)) :-
    Type \= action,
    Type \= intermediate,
    Type \= unbelieve,
    debug(solver/solve/info, '[SOLVER] considering fact goal ~w(\'~w\' - \'~w\' - \'~w\').', [Type, S, P, O]),
    ontology_reader:read_rdf(Type, S, P, O).
solve(goal(Type, S, P, O)) :-
    Type \= event,
    rule_reader:rule(_Name, Conditions, goal(Type, S, P, O)),
    debug(solver/solve/info, '[SOLVER] considering rule goal ~w(\'~w\' - \'~w\' - \'~w\').', [Type, S, P, O]),
    debug(solver/solve/info, '[SOLVER] required conditions are :[ ~w ].', [Conditions]),
    solve(Conditions).
solve(CompoundTerm) :-
    CompoundTerm =.. [Functor, S, P, O],
    Functor \= and,
    Functor \= or,
    Functor \= not,
    Functor \= true,
    Functor \= goal,
    Functor \= expression,
    debug(solver/solve/info, '[SOLVER] pack type ~w in goal format.', [Functor]),
    solve(goal(Functor, S, P, O)).
solve(condition(Type, S, P, O)) :-
    Type \= action,
    Type \= unbelieve,
    debug(solver/solve/info, '[SOLVER] convert type ~w in from condition to goal.', [Type]),
    solve(goal(Type, S, P, O)).


solve_unbelieves(Unbelieves) :-
    findall(unbelieve(S, P, O) ,solve(goal(unbelieve, S, P, O)), Unbelieves).

solve_believes(Believes) :-
    findall(believe(S, P, O) ,solve(goal(believe, S, P, O)), Believes).

solve_actions(Actions) :-
    findall(action(S, P, O) ,solve(goal(action, S, P, O)), Actions).

solve_all(Conclusions) :-
    findall(goal(Type, S, P, O) ,solve(goal(Type, S, P, O)), Conclusions).

solve_event(Event, EventData, _EventDataType, ResponseActions) :-
    debug(info, '[SOLVER] Running solver with event ~w .', [Event]),
    debug(info, '[SOLVER] Running solver with event data: ~w', [EventData]),
    ontology_reader:delete_rdf(event, _S, _P, _O),
    ontology_reader:load_ontology_string(event, EventData),
    debug(info, '[SOLVER] solve all...', []),
    solve_unbelieves(Unbelieves),
    solve_believes(Believes), 
    solve_actions(Actions),
    append([Unbelieves, Believes, Actions], Conclusions),
    debug(info, '[SOLVER] process actions: ~w', [Actions]),
    process_conclusions(Conclusions, UnfilteredResponseActions),
    debug(solver/solve_event/debug, '[SOLVER] conclusions processed, now filtering actions: ~w', [UnfilteredResponseActions]),
    filter_actions(UnfilteredResponseActions, ResponseActions),
    debug(info, '[SOLVER] actions concluded: ~w', [ResponseActions]).
solve_event(Event, _EventData, _EventDataType, []) :-
    debug(info, '[SOLVER][WARN] solver failed for event ~w, no conclusions submitted.', [Event]).

filter_actions([], []).
filter_actions([ none | UnfilteredResponseActions], ResponseActions) :-
    filter_actions(UnfilteredResponseActions, ResponseActions).
filter_actions([ Action | UnfilteredResponseActions], [ Action | ResponseActions]) :-
    Action = action(_Name, _Turtle, _Format),
    filter_actions(UnfilteredResponseActions, ResponseActions).


process_conclusions([], []).
process_conclusions([ Conclusion | Conclusions ], [ Action | Actions ]) :-
    debug(info, '[SOLVER][PROCESSING] Processing Conclusion ~w.', [Conclusion]),
    process_conclusion(Conclusion, Action),
    debug(info, '[SOLVER][PROCESSING] Processed Conclusion ~w into action ~w.', [Conclusion, Action]),
    process_conclusions(Conclusions, Actions).

process_conclusion(unbelieve(S, P, O), none) :-
    \+ ontology_reader:read_rdf(believe, S, P, O),
    !,
    debug(info, '[SOLVER][SKIP-RETRACT] Concluded unbelievetriple, but ~w was not in believe set.', [P]).
process_conclusion(unbelieve( S, P, O), none) :-
    ontology_reader:delete_rdf(believe, S, P, O),
    !,
    debug(info, '[SOLVER][RETRACT] Concluded unbelieve triple and retracting ~w from the believe set.', [P]).
process_conclusion(believe(S, P, O), none) :-
    ontology_reader:read_rdf(believe, S, P, O),
    !,
    debug(info, '[SOLVER][SKIP-ASSERT] Concluded believe triple, but it was already part of the believe set.', []).
process_conclusion(believe(S, P, O), none) :-
    ontology_reader:write_rdf(believe, S, P, O),
    !,
    debug(info, '[SOLVER][ASSERT] Concluded believe triple of predicate ~w and wrote it to believe set.', [P]).
process_conclusion(action(S, P, O), Action) :-
    !,
    debug(info, '[SOLVER][ACTION] Mapping subject ~w to short form', [S]),
    map_uri(S, SubjectRoot),
    debug(info, '[SOLVER][ACTION] Mapped subject ~w to short form ~w ', [S, SubjectRoot]),
    debug(info, '[SOLVER][ACTION] Mapping predicate ~w to short form', [P]),
    map_uri(P, PredicateRoot),
    debug(info, '[SOLVER][ACTION] Mapped predicate ~w to short form ~w ', [P, PredicateRoot]),
    debug(info, '[SOLVER][ACTION] building turtle string.', []),
    build_turtle(goal(action, S, P, O), Turtle),
    debug(info, '[SOLVER][ACTION] turtle string "~w"', [Turtle]),
    format(atom(Name), '~w.action.~w', [SubjectRoot, PredicateRoot]),
    Action = action(Name, Turtle, 'text/turtle'),
    debug(info, '[SOLVER][ACTION] Concluded action triple: \'<s>\' - \'~w\' - \'<o>\'', [ P]).
process_conclusion(Goal, none) :-
    debug(info, '[SOLVER][SKIP-OTHER] Concluded ~w. This was an unknown processing type so it was ignored.', [Goal]).


map_uri(Uri, Root) :-
    atom_length('http://www.tudelft.nl/ewi/iuxe#', Start),
    sub_atom(Uri, Start, _Len, 0, Root).

build_turtle(goal(action, S, P, O), Turtle) :-
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
    debug(info, '[WARNING] unknown object type for ~w', [O]),
    format(atom(Turtle), '<~w> <~w> <~w> .', [S, P, O]).
