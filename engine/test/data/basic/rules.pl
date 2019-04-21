:- module(rules, []).

:- op(1200,	xfy	, then).
:- op(1100,	xfy	, or).
:- op(1000, xfy , and).
:- op(900, fy , not).

rule(basic_rule_1, 
    event(agent-intents-nothing) 
    then 
    action(user-do-nothing)).

rule(basic_rule_2, 
    event(agent-intents-nothing) 
    and
    believe(Song-name-Name)
    then 
    action(user-play-Name)).

rule(basic_rule_3, 
    event(agent-intents-Something) 
    then 
    intermediate(chain-intent-Something)).

rule(basic_rule_4, 
    intermediate(chain-intent-Something)
    then 
    action(chain-intermediate-Something)).

rule(basic_rule_5, 
    event(agent-intents-nothing) 
    then 
    believe(believe_power-over-9000)).

rule(basic_rule_6, 
    event(agent-intents-nothing) 
    then 
    unbelieve(Song-preview_url-Obj)).

rule(basic_rule_7, 
    event(agent-has-second_pass) 
    then 
    action(second_pass-at-1)).