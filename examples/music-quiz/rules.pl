:- module(rules, []).

:- op(1200,	xfy	, then).
:- op(1100,	xfy	, or).
:- op(1000, xfy , and).
:- op(900, fy , not).

rule(waiting_to_start, true then believe(agent-intends-waiting_to_start)).

rule(ask_question_like_song, true then believe(agent-intends-ask_question_like_song)).
rule(ask_question_full_song, true then believe(agent-intends-ask_question_full_song)).
rule(ask_question_start_quiz, true then believe(agent-intends-ask_question_start_quiz)).
rule(ask_question_music_question, true then believe(agent-intends-ask_question_music_question)).
rule(ask_question_quiz_difficulty, true then believe(agent-intends-ask_question_quiz_difficulty)).

rule(wait_for_answer_like_song, true then believe(agent-intends-wait_for_answer_like_song)).
rule(wait_for_answer_full_song, true then believe(agent-intends-wait_for_answer_full_song)).
rule(wait_for_answer_start_quiz, true then believe(agent-intends-wait_for_answer_start_quiz)).
rule(wait_for_answer_music_question, true then believe(agent-intends-wait_for_answer_music_question)).
rule(wait_for_answer_quiz_difficulty, true then believe(agent-intends-wait_for_answer_quiz_difficulty).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Rules executed when the agent starts. 
%%
%% This requires the presentence of Pepper as it sets the listen command to allow Pepper to
%% hear cerain words for the entire quiz, though some event may be ignored based on the current
%% intent
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rule(agent_starts1,
    event(agent-is-ready) then believe(agent-intents-waiting_to_start)
).

rule(agent_starts2,
    event(agent-is-ready) then action(pepper-listen-"Ja, Nee, A, B, C, Begin, Stop, Eenvoudig, Normaal, Moeilijk")
).

rule(agent_starts3,
    event(agent-is-ready) and believe(text-question_start-StartText) then action(pepper-say-StartText)
).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Rules for starting the quiz. 
%%
%% There are two requirements for starting the quiz. First the agent should be intent on
%% 'waiting_to_start' and second either somebody should enter zone 1 or say 'begin' to pepper.
%%
%% When this happens the agent will ask the begin_quiz_question and wait for either a yes, no or
%% timeout. "No" and a timeout will put pepper back in "waiting_to_start".
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rule(start_rule1,
    believe(agent-intents-waiting_to_start) and event(Word-word-"Begin")
    then
    intermediate(agent-asks-start_quiz)
).
rule(start_rule1,
    believe(agent-intents-waiting_to_start) and event(Person-enters-area1)
    then
    intermediate(agent-asks-start_quiz)
).

rule(start_rule2,
    intermediate(agent-asks-start_quiz) and believe(text-question_start_quiz-StartQuiz)
    then 
    action(pepper-say-StartQuiz)
).

rule(start_rule3,
    intermediate(agent-asks-start_quiz) 
    then 
    unbelieve(agent-intents-Something)
).

rule(start_rule4,
    intermediate(agent-asks-start_quiz) 
    then 
    believe(agent-intents-wait_for_answer_start_quiz)
).

rule(start_rule4,
    intermediate(agent-asks-start_quiz) 
    then 
    action(timer-timeout-25000)
).