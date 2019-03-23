:- module(rule_transformer, []).

:- use_module(library(semweb/rdf11)).
:- use_module(library(debug)).

%% 
%% Initializes the prefixes used by the rules.
%%
initialize_prefixes :-
    rdf_register_prefix(iuxe, 'http://www.tudelft.nl/ewi/iuxe#', [keep(true)]),
    rdf_register_prefix(believe, 'http://www.tudelft.nl/ewi/iuxe/believes#', [keep(true)]),
    rdf_register_prefix(action, 'http://www.tudelft.nl/ewi/iuxe/actions#', [keep(true)]),
    rdf_register_prefix(event, 'http://www.tudelft.nl/ewi/iuxe/events#', [keep(true)]).

%%
%% The default prefix when omited.
%%
default_prefix(iuxe).

%%
%% Valid goals in a rule.
%%
valid_goal(action(_S-_P-_O)) :- !.
valid_goal(believe(_S-_P-_O)) :- !.
valid_goal(unbelieve(_S-_P-_O)) :- !.
valid_goal(itermediate(_S-_P-_O)) :- !.

%%
%% Valid terms in a condition.
%%
valid_condition_term(event(_S-_P-_O)) :- !.
valid_condition_term(believe(_S-_P-_O)) :- !.
valid_condition_term(itermediate(_S-_P-_O)) :- !.

%%
%% Valid operators in a condition.
%%
valid_condition_operator(and(_Left, _Right)) :- !.
valid_condition_operator(or(_Left, _Right)) :- !.
valid_condition_operator(not(_Left)) :- !.

valid_condition_expression(Term) :- expression(Term).

%%
%% Valid expressions
%%
expression(_X > _Y). %% True if expression Expr1 evaluates to a larger number than Expr2.
expression(_X < _Y). %% True if expression Expr1 evaluates to a smaller number than Expr2.
expression(_X =< _Y). %% True if expression Expr1 evaluates to a smaller or equal number to Expr2.
expression(_X >= _Y). %% True if expression Expr1 evaluates to a larger or equal number to Expr2.
expression(_X =\= _Y). %% True if expression Expr1 evaluates to a larger or equal number to Expr2.
expression(_X =:= _Y). %% True if expression Expr1 evaluates to a number non-equal to Expr2.
expression(_X is _Y). %% True if expression Expr1 evaluates to a number equal to Expr2.
expression(_X = _Y).  %% True if expression Expr1 *unifies* witho Expr2.

%%
%% Test if a Term is a valid literal
%%
literal(Term) :- string(Term).
literal(Term) :- number(Term).

%%
%% Test is a term is a valid uri component
%%
expanded_uri(Term) :- 
    atom(Term),
    sub_atom(Term, 0, 7, _After, 'http://').
expanded_uri(Term) :- 
    atom(Term),
    sub_atom(Term, 0, 8, _After, 'https://').

%%
%% Test wether a term is a prefixed uri of the form
%%   <prefix:term>
%%
prefixed_uri(Prefix:Term) :- 
    atom(Prefix), 
    atom(Term).

%%
%% Test wether a term is a uri fragment without prefix.
%%
prefixable_uri(Term) :-
    atom(Term),
    \+ sub_atom(Term, 0, 7, _After1, 'http://'),
    \+ sub_atom(Term, 0, 8, _After2, 'https://').

expand_term(Term, ExpandedTerm) :-
    ground(Term),
    prefixed_uri(Term),
    rdf_global_id(Term, ExpandedTerm).

%%
%% Expands a term to its complete uri.
%%
expand_term_uri(Term, ExpandedTerm) :-
    ground(Term),
    prefixed_uri(Term),
    rdf_global_id(Term, ExpandedTerm).
expand_term_uri(Term, ExpandedTerm) :-
    ground(Term),
    prefixable_uri(Term),
    default_prefix(Prefix),
    rdf_global_id(Prefix:Term, ExpandedTerm).
expand_term_uri(Term, Term).

%%
%% Transform a goal of the type functor(Subject, Predicate, Object) into
%% goal(functor, <expanded-sub>, <expanded-pred>, <expanded-obj>)
%%
transform_goal(Goal, TransformedGoal) :-
    valid_goal(Goal),
    !,
    Goal =.. [Type, Subject-Predicate-Object ],
    expand_term_uri(Subject, ExSubject),
    expand_term_uri(Predicate, ExPredicate),
    expand_term_uri(Object, ExObject),
    TransformedGoal = goal(Type, ExSubject, ExPredicate, ExObject).
transform_goal(Goal, _) :-
    !,
    throw( error(invalid_goal, Goal) ).

%%
%% Transforms a rule condition into a 
%%
transform_condition(true, true).
transform_condition(and(Left, Right), and(TrLeft, TrRight)) :-
    transform_condition(Left, TrLeft),
    transform_condition(Right, TrRight).
transform_condition(or(Left, Right), or(TrLeft, TrRight)) :-
    transform_condition(Left, TrLeft),
    transform_condition(Right, TrRight).
transform_condition(not(Term), not(TrTerm)) :-
    transform_condition(Term, TrTerm).
transform_condition(Term, expression(Term)) :-
    expression(Term).
transform_condition(Term, TransformedTerm) :-
    valid_condition_term(Term),
    Term =.. [Type, Subject-Predicate-Object ],
    expand_term_uri(Subject, ExSubject),
    expand_term_uri(Predicate, ExPredicate),
    expand_term_uri(Object, ExObject),
    TransformedTerm = condition(Type, ExSubject, ExPredicate, ExObject).
transform_condition(Cond, _) :-
    throw( error(invalid_condition, Cond) ).

%%
%% Transform a then functor into and expanded condition and goal
%%
transform_then(then(Cond, Goal), then(TrCond, TrGoal)) :-
    transform_condition(Cond, TrCond),
    transform_goal(Goal, TrGoal).
transform_then(Then, _TrThen) :-
    throw( error(invalid_then_func, Then) ).

%%
%% Transform rule into an expand rule.
%%
transform(rule(Name, Then), rule(Name, TrCond, TrGoal)) :-
    transform_then(Then, TrThen),
    TrThen = then(TrCond, TrGoal).
transform(rule(Name, Rule), _) :-
    throw( error(invalid_rule, Name, Rule) ).



%%
%% Tests
%%
:- begin_tests(rule_transformer).

test(expand_term_atom, [ true(Result = 'http://www.tudelft.nl/ewi/iuxe#agent') , nondet]) :-
    initialize_prefixes,
    expand_term_uri(agent, Result).

test(expand_term_prefix_atom, [ true(Result = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'), nondet ]) :-
    initialize_prefixes,
    expand_term_uri(rdf:type, Result).

test(expand_term_uri, [ true(Result = 'http://www.tudelft.nl/ewi/iuxe#agent') ]) :-
    initialize_prefixes,
    expand_term_uri('http://www.tudelft.nl/ewi/iuxe#agent', Result).

test(expand_term_int, [ true(Result = 20) ]) :-
    initialize_prefixes,
    expand_term_uri(20, Result).

test(expand_term_float, [ true(Result = 2.5) ]) :-
    initialize_prefixes,
    expand_term_uri(2.5, Result).

test(expand_term_string, [ true(Result = "Hello") ]) :-
    initialize_prefixes,
    expand_term_uri("Hello", Result).

test(expand_term_var, [ true(var(Result)) ]) :-
    initialize_prefixes,
    expand_term_uri(_X, Result).

test(transform_goal_atom, [ true(Goal = goal(believe,
    'http://www.tudelft.nl/ewi/iuxe#winter',
    'http://www.tudelft.nl/ewi/iuxe#is',
    'http://www.tudelft.nl/ewi/iuxe#coming')), nondet ]) :-
    initialize_prefixes,
    transform_goal(believe(winter-is-coming), Goal).

test(transform_condition_atom, [ true(Cond = condition(believe,
    'http://www.tudelft.nl/ewi/iuxe#winter',
    'http://www.tudelft.nl/ewi/iuxe#is',
    'http://www.tudelft.nl/ewi/iuxe#coming')), nondet ]) :-
    initialize_prefixes,
    transform_condition(believe(winter-is-coming), Cond).

test(transform_condition_atom_var, [ true(unifiable(
        Cond, 
        condition(believe,
            'http://www.tudelft.nl/ewi/iuxe#winter',
            'http://www.tudelft.nl/ewi/iuxe#is',
            _), _Unifier)), nondet ]) :-
    initialize_prefixes,
    transform_condition(believe(winter-is-_X), Cond).

test(transform_condition_atom_float, [ true(unifiable(
        Cond, 
        condition(believe,
            'http://www.tudelft.nl/ewi/iuxe#winter',
            'http://www.tudelft.nl/ewi/iuxe#is',
            2.3), _Unifier)), nondet ]) :-
    initialize_prefixes,
    transform_condition(believe(winter-is-2.3), Cond).

test(transform_condition_atom_float, [ true(unifiable(
        Cond, 
        condition(believe,
            "Hello",
            _Y,
            2.3), _Unifier)), nondet ]) :-
    initialize_prefixes,
    transform_condition(believe("Hello"-_X-2.3), Cond).

test(transform_condition_event, [ true(
    Cond
        ==
    condition(event, 
        'http://www.tudelft.nl/ewi/iuxe#agent', 
        'http://www.tudelft.nl/ewi/iuxe#saw_person_entering', Person)
), nondet ]) :-
    initialize_prefixes,
    transform_condition(event(agent-saw_person_entering-Person), Cond).

test(transform_condition_believe2, [ true(
    Cond
        ==
    condition(believe, 
        'http://www.tudelft.nl/ewi/iuxe#agent', 
        'http://www.tudelft.nl/ewi/iuxe#sees_persons', Number)
), nondet ]) :-
    initialize_prefixes,
    transform_condition(believe(agent-sees_persons-Number), Cond).

test(transform_condition_expression, [ true(
    Cond
        ==
    expression(NextNumber is Number + 1)
), nondet ]) :-
    initialize_prefixes,
    transform_condition(NextNumber is Number + 1, Cond).

test(transform_condition_and, [ true(
    Cond
        ==
    and(
        condition(believe, 
            'http://www.tudelft.nl/ewi/iuxe#agent', 
            'http://www.tudelft.nl/ewi/iuxe#sees_persons', Number),
        expression(NextNumber is Number + 1)
    )
), nondet ]) :-
    initialize_prefixes,
    B = believe(agent-sees_persons-Number),
    Ex = (NextNumber is Number + 1),
    And = and(B, Ex),
    transform_condition(And, Cond).

test(transform_condition_and_and, [ true(
    Cond
        ==
    and(
        condition(event, 'http://www.tudelft.nl/ewi/iuxe#agent', 
            'http://www.tudelft.nl/ewi/iuxe#saw_person_entering', Person),
        and(
            condition(believe, 
                'http://www.tudelft.nl/ewi/iuxe#agent', 
                'http://www.tudelft.nl/ewi/iuxe#sees_persons', Number),
            expression(NextNumber is Number + 1)
        )
    )
), nondet ]) :-
    initialize_prefixes,
    E = event(agent-saw_person_entering-Person),
    B = believe(agent-sees_persons-Number),
    Ex = (NextNumber is Number + 1),
    And = and(E, and(B, Ex)),
    transform_condition(And, Cond).

test(transform_then, [ true(
    TransformedThen
        ==
    then(
        and(
            condition(event, 'http://www.tudelft.nl/ewi/iuxe#agent', 
                'http://www.tudelft.nl/ewi/iuxe#saw_person_entering', Person),
            and(
                condition(believe, 'http://www.tudelft.nl/ewi/iuxe#agent', 
                    'http://www.tudelft.nl/ewi/iuxe#sees_persons', Number),
                expression(NextNumber is Number + 1)
            )
        ),
        goal(believe, 'http://www.tudelft.nl/ewi/iuxe#agent', 
            'http://www.tudelft.nl/ewi/iuxe#sees_persons', NextNumber)
    )
), nondet ]) :-

    initialize_prefixes,
    E = event(agent-saw_person_entering-Person),
    B = believe(agent-sees_persons-Number),
    Ex = (NextNumber is Number + 1),
    And = and(E, and(B, Ex)),
    G = believe(agent-sees_persons-NextNumber),
    T = then(And, G),
    transform_then(T, TransformedThen).

test(transform_rule_complex, [ true(
    TransformedRule
        ==
    rule(
        test_rule273,
        and(
            condition(event, 'http://www.tudelft.nl/ewi/iuxe#agent', 
                'http://www.tudelft.nl/ewi/iuxe#saw_person_entering', Person),
            and(
                condition(believe, 'http://www.tudelft.nl/ewi/iuxe#agent', 
                    'http://www.tudelft.nl/ewi/iuxe#sees_persons', Number),
                expression(NextNumber is Number + 1)
            )
        ),
        goal(believe, 'http://www.tudelft.nl/ewi/iuxe#agent', 
            'http://www.tudelft.nl/ewi/iuxe#sees_persons', NextNumber)
    )
), nondet ]) :-

    initialize_prefixes,
    E = event(agent-saw_person_entering-Person),
    B = believe(agent-sees_persons-Number),
    Ex = (NextNumber is Number + 1),
    And = and(E, and(B, Ex)),
    G = believe(agent-sees_persons-NextNumber),
    T = then(And, G),
    R = rule(test_rule273, T),
    transform(R, TransformedRule).

:- end_tests(rule_transformer).