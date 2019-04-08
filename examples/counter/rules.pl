:- module(rules, []).

:- op(1200,	xfy	, then).
:- op(1100,	xfy	, or).
:- op(1000, xfy , and).
:- op(900, fy , not).

rule(rule1,
        event(agent-is-ready)
    then
        action(timer-set_interval-"10000/every_10")
).

rule(rule2,
        event(timer-set-"every_10")
    and
        believe(formats-interval_set-Format)
    and 
        format(string(Text), Format, [])
    then
        action(pepper-say-Text)
).

rule(rule2a,
        event(timer-triggered-"every_10")
    and
        believe(agent-counted_to-N)
    then
        intermediate(count-was-N)
).

rule(rule2b,
        intermediate(count-was-N)
    and
        M is N + 1
    then
        intermediate(count-is-M)
).

rule(rule2c,
        intermediate(count-is-N)
    then
        believe(agent-counted_to-N)
).

rule(rule2d,
        intermediate(count-was-N)
    then
        unbelieve(agent-counted_to-N)
).

rule(rule2e,
        intermediate(count-is-N)
    and 
        believe(formats-next_number-Format)
    and
        format(string(S), Format, [N])
    then
        action(pepper-say-S)
).

rule(rule3,
        intermediate(count-is-N)
    and
        N > 9
    then
        intermediate(count-stops_at-N)
).

rule(rule3a,
        intermediate(count-stops_at-N)
    then
        action(timer-clear_interval-"every_10")
).

rule(rule3b,
        intermediate(count-stops_at-N)
    and 
        believe(formats-last_number-Format)
    and
        format(string(S), Format, [N])
    then
        action(pepper-say-S)
).
