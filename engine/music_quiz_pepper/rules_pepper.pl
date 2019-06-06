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

rule(ready_03,
    event(agent-is-ready) then
    action(pepper-show-"http://192.168.100.175:3000")).

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
    intermediate(quiz-finished-setup) and
    intermediate(user-clicked-rules) then
        intermediate(quiz-start-rules)).

rule(rules_02,
    intermediate(quiz-finished-game) and
    intermediate(user-clicked-rules) then
        intermediate(quiz-start-rules)).


%%
%% Rules start game
%%

rule(start_game_01,
    intermediate(quiz-finished-rules) and
    intermediate(user-clicked-start) then
        intermediate(quiz-start-game) ).


rule(start_game_02,
    intermediate(quiz-start-game) then
        action(pepper-say-"Let's start the game!")).

%%
%% Handle tablet inputs
%%

rule(tablet_01,
    event(tablet-clicked-start_button) then
        intermediate(user-clicked-start)).

rule(tablet_02,
    event(tablet-clicked-rules_button) then
        intermediate(user-clicked-rules)).

rule(tablet_03,
    event(tablet-clicked-stop_button) then
        intermediate(user-clicked-stop)).

rule(tablet_04,
    event(tablet-clicked-yes) then
        intermediate(user-clicked-yes)).

rule(tablet_05,
    event(tablet-clicked-no) then
        intermediate(user-clicked-no)).

rule(tablet_04,
    event(spotify-info-Artistinfo) then
        action(tablet-artist_info-Artistinfo)).

rule(artist,
    event(spotify-info-Artistinfo) then
          action(pepper-say-Artistinfo)).

rule(end_game,
    event(spotify-end_game-Data) then
        action(pepper-say-"Thanks for playing")).



%%
%% Chain topics when previous topic finsihed.
%%

rule(starting_02,
    intermediate(quiz-start-Topic) then
        intermediate(quiz-setup-Topic)).

%%
%% Setup a question or information
%%
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

%%
%% Finish setup
%%
rule(setup_19,
    believe(quiz-state-setup) then
        intermediate(quiz-finished-setup)).

rule(setup_20,
    believe(quiz-state-rules) then
        intermediate(quiz-finished-rules)).



rule(setup_23,
    intermediate(user-clicked-stop) then
        intermediate(quiz-finished-game)).

rule(setup_22,
    event(spotify-end_game-Data) then
        intermediate(quiz-finished-game)).

rule(test_1,
intermediate(quiz-can_finish-game) then
action(pepper-say-"almost finished")).

rule(test_2,
intermediate(quiz-finished-game) then
action(pepper-say-"finished for sure")).

%% rule(setup_21,
%%    believe(quiz-state-game) then
%%        intermediate(quiz-can_finish-game)).

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

%% rule(speech_02,
%%    intermediate(song-played-more) then
%%        action(pepper-say-"Shall I play more?")).

rule(speech_03,
    intermediate(song-played-more) and
    intermediate(user-clicked-yes) then
        action(spotify-continue-music)).