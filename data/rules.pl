:- module(rules, [rule/2]).

:- op(1200,	xfy	, then).
:- op(1100,	xfy	, or).
:- op(1000, xfy , and).
:- op(900, fy , not).

rule(agent_starts,
        event(agent-is-ready)
    then
        action(pepper-say-"Hallo, laten we muziek maken.")
).