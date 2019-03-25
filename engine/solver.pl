:- module(solver, []).

:- use_module(library(debug)).

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
    ontology_reader:read_rdf(Type, S, P, O),
    debug(solver/info, '[SOLVER] solve read database for ~w  and got \'~w\' - \'~w\' - \'~w\'.', [Type, S, P, O]).
solve(goal(Type, S, P, O)) :-
    rule_reader:rule(_Name, Conditions, goal(Type, S, P, O)),
    solve(Conditions).
solve(CompoundTerm) :-
    CompoundTerm =.. [Functor, S, P, O],
    Functor \= and,
    Functor \= or,
    Functor \= not,
    Functor \= true,
    Functor \= goal,
    Functor \= expression,
    solve(goal(Functor, S, P, O)).


solve_unbelieves(Unbelieves) :-
    findall(unbelieve(S, P, O) ,solve(goal(unbelieve, S, P, O)), Unbelieves).

solve_believes(Believes) :-
    findall(believe(S, P, O) ,solve(goal(believe, S, P, O)), Believes).

solve_actions(Actions) :-
    findall(action(S, P, O) ,solve(goal(action, S, P, O)), Actions).

solve_all(Conclusions) :-
    findall(goal(Type, S, P, O) ,solve(goal(Type, S, P, O)), Conclusions).

solve_event(Event, EventData, _EventDataType, ResponseActions) :-
    debug(info, '[SOLVER] Running solver with event ~w', [Event]),
    ontology_reader:delete_rdf(event, _S, _P, _O),
    ontology_reader:load_ontology_string(event, EventData),

    debug(info, '[SOLVER] solve all...', []),
    solve_all(Conclusions),
    debug(info, '[SOLVER] process conclusions: ~w', [Conclusions]),
    process_conclusions(Conclusions, ResponseActions).

process_conclusions([], []).
process_conclusions([ Conclusion | Conclusions ], [ Action | Actions ]) :-
    process_conclusion(Conclusion, Action),
    process_conclusions(Conclusions, Actions).

process_conclusion(goal(unbelieve, S, P, O), none) :-
    \+ ontology_reader:read_rdf(believe, S, P, O),
    !,
    debug(info, '[SOLVER][SKIP-RETRACT] Concluded unbelievetriple, but ~w was not in believe set.', [P]).
process_conclusion(goal(unbelieve, S, P, O), none) :-
    ontology_reader:delete_rdf(believe, S, P, O),
    !,
    debug(info, '[SOLVER][RETRACT] Concluded unbelieve triple and retracting ~w from the believe set.', [P]).
process_conclusion(goal(believe, S, P, O), none) :-
    ontology_reader:read_rdf(believe, S, P, O),
    !,
    debug(info, '[SOLVER][SKIP-ASSERT] Concluded believe triple, but it was already part of the believe set.', []).
process_conclusion(goal(believe, S, P, O), none) :-
    ontology_reader:write_rdf(believe, S, P, O),
    !,
    debug(info, '[SOLVER][ASSERT] Concluded believe triple of predicate ~w and wrote it to believe set.', [P]).
process_conclusion(goal(action, S, P, O), Action) :-
    map_uri(S, SubjectRoot),
    map_uri(P, PredicateRoot),
    build_turtle(goal(action, S, P, O), Turtle),
    format(atom(Name), '~w.action.~w', [SubjectRoot, PredicateRoot]),
    Action = action(Name, Turtle, 'text/turtle'),
    debug(info, '[SOLVER][ACTION] Concluded action triple: \'<s>\' - \'~w\' - \'<o>\'', [ P]).
process_conclusion(goal(Type, _S, P, _O), none) :-
    debug(info, '[SOLVER][SKIP-OTHER] Concluded ~w triple. ~w triples need no processing and are ignored.', [Type, P]).


map_uri(Uri, Root) :-
    sub_atom(Uri, 0, _Len, Root, 'http://www.tudelft.nl/ewi/iuxe#').

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
