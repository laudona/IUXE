:- module(rules, []).

:- op(1200,	xfy	, then).
:- op(1100,	xfy	, or).
:- op(1000, xfy , and).
:- op(900, fy , not).

rule(rule1,
        event(agent-is-ready)
    then
        action(timer-set_timeout-"10000/wait_10")
).

rule(rule3,
        event(timer-set-"wait_10")
    and
        believe(texts-time_set-Text)
    then
        action(pepper-say-Text)
).

rule(rule3,
        event(timer-triggered-"wait_10")
    and
        believe(texts-time_ready-Text)
    then
        action(pepper-say-Text)
).