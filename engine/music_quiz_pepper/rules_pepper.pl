:- module(rules, []).

:- op(1200,	xfy	, then).
:- op(1100,	xfy	, or).
:- op(1000, xfy , and).
:- op(900, fy , not).

%%
%% Starting the application
%%

rule(ready_01,
    event(agent-is-ready) then
    action(pepper-listen-"Ja, Nee, Beginnen, Stoppen, Eenvoudig, Reglement")).

rule(ready_02,
    event(agent-is-ready) then
        intermediate(quiz-start-waiting)).

%%
%% Rules explanation
%%

rule(rules_01,
    intermediate(setup-is-finished) and
    event(pepper-heard-WordId) and
    event(WordId-word-"Reglement")  then
        intermediate(quiz-explain-rules) ).

rule(rules_02,
    intermediate(quiz-explain-rules) then
        action(pepper-say-"Hello, welcome to Bingo.")).

rule(rules_03,
    intermediate(quiz-explain-rules) and
    event(pepper-said-"Hello, welcome to Bingo.") then
        intermediate(quiz-explained-rules)).

%%
%% Rules start game
%%

rule(start_game,
    event(pepper-heard-WordId) and
    event(WordId-word-"Beginnen") then
        intermediate(quiz-can_start-game) ).

%%
%% States
%%

rule(state_01,
    intermediate(quiz-explained-rules)  and
    intermediate(quiz-can_start-game) then
        intermediate(quiz-start-game)).

rule(state_02,
    intermediate(quiz-start-game) or
    intermediate(quiz-play-next_song) then
        intermediate(quiz-play-song)).

rule(state_03,
    intermediate(quiz-play-song) and
    intermediate(timer-has-elapsed) then
        intermediate(quiz-show-info)).

rule(state_04,
    event(pepper-detected-speech) and
    intermediate(quiz-play-song) then
        intermediate(quiz-continue-song)).

rule(state_05,
    intermediate(quiz-show-info) and
    not intermediate(quiz-continue-song) then
        intermediate(quiz-play-next_song)).

rule(state_06,
    intermediate(quiz-show-info) and
    intermediate(quiz-finished-song) then
        intermediate(quiz-play-next_song)).

%%
%% Handle pepper word inputs
%%

rule(pepper_01,
    event(pepper-heard-Wid) and
    event(Wid-word-"Ja") then
        intermediate(user-said-yes)).
rule(pepper_02,
    event(pepper-heard-Wid) and
    event(Wid-word-"Nee") then
        intermediate(user-said-no)).
rule(pepper_03,
    event(pepper-heard-Wid) and
    event(Wid-word-"Beginnen") then
        intermediate(user-said-start)).
rule(pepper_04,
    event(pepper-heard-Wid) and
    event(Wid-word-"Stoppen") then
        intermediate(user-said-stop)).
rule(pepper_05,
    event(pepper-said-Word) then
        intermediate(pepper-said-word)).


%%
%% Handle tablet inputs
%%

%%
%% Chain topics when previous topic finsihed.
%%
rule(starting_01,
    intermediate(quiz-finished-Topic) and
    believe(Topic-is_followed_by-OtherTopic) then
        intermediate(quiz-setup-OtherTopic)).

rule(starting_02,
    intermediate(quiz-start-Topic) then
        intermediate(quiz-setup-Topic)).

%%
%% Setup a question or information
%%
rule(setup_01,
    intermediate(quiz-setup-Topic) and
    believe(Topic-title-Text) then
        action(tablet-title-Text)).

rule(setup_02,
    intermediate(quiz-setup-Topic) and
    believe(Topic-main-Text) then
        action(tablet-main-Text)).

rule(setup_03,
    intermediate(quiz-setup-Topic) and
    believe(Topic-support-Text) then
        action(tablet-support-Text)).

rule(setup_04,
    intermediate(quiz-setup-Topic) and
    believe(Topic-button_a_text-Text) then
        action(tablet-button_a_text-Text)).

rule(setup_05,
    intermediate(quiz-setup-Topic) and
    believe(Topic-button_b_text-Text) then
        action(tablet-button_b_text-Text)).

rule(setup_06,
    intermediate(quiz-setup-Topic) and
    believe(Topic-button_c_text-Text) then
        action(tablet-button_c_text-Text)).

rule(setup_07,
    intermediate(quiz-setup-Topic) and
    believe(Topic-button_a_text-Text) then
        action(tablet-show-button_a_text)).

rule(setup_08,
    intermediate(quiz-setup-Topic) and
    believe(Topic-button_b_text-Text) then
        action(tablet-show-button_b_text)).

rule(setup_09,
    intermediate(quiz-setup-Topic) and
    believe(Topic-button_c_text-Text) then
        action(tablet-show-button_c_text)).

rule(setup_10,
    intermediate(quiz-setup-Topic) and
    not believe(Topic-button_a_text-Text) then
    action(tablet-hide-button_a_text)).

rule(setup_11,
    intermediate(quiz-setup-Topic) and
    not believe(Topic-button_b_text-Text) then
        action(tablet-hide-button_b_text)).

rule(setup_12,
    intermediate(quiz-setup-Topic) and
    not believe(Topic-button_c_text-Text) then
        action(tablet-hide-button_c_text)).

rule(setup_13,
    intermediate(quiz-setup-Topic) and
    believe(Topic-say_during_setup-Text) then
        action(pepper-say-Text)).

rule(setup_14,
    intermediate(quiz-setup-Topic) then
        believe(quiz-current-Topic)).

rule(setup_15,
    intermediate(quiz-setup-Topic) then
        believe(quiz-state-setup)).

rule(setup_16,
    intermediate(quiz-setup-Topic) then
        unbelieve(quiz-current-OtherTopic)).

rule(setup_17,
    intermediate(quiz-setup-Topic) then
        unbelieve(quiz-state-OtherState)).

rule(setup_answered,
    intermediate(quiz-setup-Topic) then
        unbelieve(user-answered-topic)).

rule(setup_18,
    intermediate(quiz-setup-Topic) and
    believe(Topic-show_page-PageUrl) then
        action(pepper-show-PageUrl)).

%%
%% Finish setup
%%
rule(setup_19,
    intermediate(pepper-said-word) and
    believe(quiz-state-setup) then
        intermediate(setup-is-finished)).

%%
%% General pepper behavior
%%
rule(greet_when_coming_close,
       event(pepper-just_arrived-Person) and
       event(Person-entered-zone1)
   then
       action(pepper-say-"Hallo daar")).
