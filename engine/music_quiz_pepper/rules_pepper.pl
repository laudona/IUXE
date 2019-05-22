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
        intermediate(quiz-start-setup)).

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
    event(pepper-heard-Wid) and
    event(Wid-word-"Reglement") then
        intermediate(user-said-rules)).

rule(pepper_06,
    event(pepper-said-Word) then
        intermediate(pepper-said-word)).


%%
%% Rules explanation
%%

rule(rules_01,
    intermediate(quiz-finished-setup) then
        intermediate(quiz-start-rules)).


%%
%% Rules start game
%%

rule(start_game_01,
    intermediate(quiz-finished-rules) then
        intermediate(quiz-start-game) ).

rule(start_game_02,
    intermediate(quiz-start-game) then
        action(pepper-say-"Let's start the game!")).

rule(start_game_02,
    intermediate(quiz-start-game) then
        action(spotify-start-song)).


%%
%% Handle tablet inputs
%%

rule(tablet_01,
    event(tablet-clicked-start) then
        intermediate(user-clicked-start)).

rule(tablet_02,
    event(tablet-clicked-rules) then
        intermediate(user-clicked-rules)).

rule(tablet_03,
    event(tablet-clicked-stop) then
        intermediate(user-clicked-stop)).

rule(tablet_04,
    event(tablet-clicked-yes) then
        intermediate(user-clicked-yes)).

rule(tablet_05,
    event(tablet-clicked-no) then
        intermediate(user-clicked-no)).

rule(tablet_04,
    event(tablet-show-Artistinfo) then
        action(pepper-say-Artistinfo)).

%%
%% Chain topics when previous topic finsihed.
%%

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
        believe(quiz-state-Topic)).

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
        intermediate(quiz-finished-setup)).

rule(setup_20,
    intermediate(pepper-said-word) and
    believe(quiz-state-rules) then
        intermediate(quiz-finished-rules)).

rule(setup_21,
    intermediate(user-clicked-stop) or
    event(spotify-end-game) and
    believe(quiz-state-game) then
        intermediate(quiz-finished-game)).

%%
%% General pepper behavior
%%
rule(greet_when_coming_close,
       event(pepper-just_arrived-Person) and
       event(Person-entered-zone1)
   then
       action(pepper-say-"Hallo daar")).


%%
%% Speech
%%
rule(speech_01,
    event(pepper-detected-speech) then
        intermediate(song-played-more)).

rule(speech_02,
    intermediate(song-played-more) then
        action(pepper-say-"Shall I play more?")).

rule(speech_03,
    intermediate(song-played-more) and
    intermediate(user-clicked-yes) then
        action(spotify-continue-music)).