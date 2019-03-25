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
    \+ solve(Left).
solve(expression(Term)) :-
    call(Term).
solve(goal(Type, S, P, O)) :-
    ontology_reader:read_rdf(Type, S, P, O),
    debug(solver/info, '[SOLVER] solve read database for ~w  and got \'~w\' - \'~w\' - \'~w\'.', [Type, S, P, O]).
solve(goal(Type, S, P, O)) :-
    rule_reader:rule(Name, Conditions, goal(Type, S, P, O)),
    solve(Conditions).
solve(CompoundTerm) :-
    CompoundTerm =.. [Functor, S, P, O],
    Functor /= and,
    Functor /= or,
    Functor /= not,
    Functor /= true,
    Functor /= goal,
    Functor /= expression,
    solve(goal(Functor, S, P, O)).


solve_unbelieves(Unbelieves) :-
    findall(unbelieve(S, P, O) ,solve(goal(unbelieve, S, P, O)), Unbelieves).

solve_believes(Believes) :-
    findall(believe(S, P, O) ,solve(goal(believe, S, P, O)), Believes).

solve_actions(Actions) :-
    findall(action(S, P, O) ,solve(goal(action, S, P, O)), Actions).

solve_all(Conclusions) :-
    findall(goal(Type, S, P, O) ,solve(goal(Type, S, P, O)), Conclusions).

solve_event(Event, EventData, EventDataType, ResponseActions) :-
    debug(info, '[SOLVER] Running solver with event ~w', [Event]),
    ontology_reader:delete_rdf(event, _S, _P, _O),
    ontology_reader:load_ontology_string(event, EventData),

    debug(info, '[SOLVER] solve all...', []),
    solve_all(Conclusions),
    debug(info, '[SOLVER] process conclusions: ~w', [Conclusions]),
    process_conclusions(Conclusions, ResponseActions).

process_conclusions([], []).
process_conclusions([ Conclusion | Conclusions ], Actions) :-
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
process_conclusion(goal(believe, S, P, O), Action) :-
    ontology_reader:read_rdf(believe, S, P, O),
    !,
    debug(info, '[SOLVER][SKIP-ASSERT] Concluded believe triple, but it was already part of the believe set.', [Type]).
process_conclusion(goal(believe, S, P, O), Action) :-
    ontology_reader:write_rdf(believe, S, P, O),
    !,
    debug(info, '[SOLVER][ASSERT] Concluded believe triple of predicate ~w and wrote it to believe set.', [P]).
process_conclusion(goal(action, S, P, O), Action) :-
    debug(info, '[SOLVER][ACTION] Concluded action triple: \'<s>\' - \'~w\' - \'<o>\'', [Type, P]).
process_conclusion(goal(Type, S, P, O), Action) :-
    debug(info, '[SOLVER][SKIP-OTHER] Concluded ~w triple. ~w triples need no processing and are ignored.', [Type, P]).


