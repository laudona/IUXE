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
%% General rules
%%

rule(rules_01,
    intermediate(quiz-finished-setup) and
    intermediate(user-clicked-rules) then
        intermediate(quiz-start-rules)).

rule(rules_02,
    intermediate(quiz-finished-rules) and
    intermediate(user-clicked-start) then
        intermediate(quiz-start-game) ).

rule(rules_03,
    intermediate(quiz-finished-game) then
        intermediate(quiz-start-setup)).

rule(start_game_welcome,
    intermediate(quiz-start-game) then
        action(pepper-say-"Laten we het spel starten!")).

rule(end_game_goodbye,
    event(spotify-end_game-Data) then
        action(pepper-say-"Bedankt voor het meespelen")).

rule(introduce_next_song,
    event(spotify-new-Song) then
        action(pepper-song-"new")).


%%
%% Handle pepper word inputs
%%event(spotify-new-Song)

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
%% Pepper behavior
%%

%% rule(behavior_01,
%%    intermediate(quiz-start-game) then
%%    action(pepper-start-"behavior")).

%% rule(behavior_02,
%%    intermediate(quiz-start-game) then
%%    action(pepper-run-"behavior")).


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
    event(tablet-clicked-skip_button) then
        intermediate(user-clicked-skip)).

rule(tablet_05,
    event(spotify-info-Artistinfo) then
        action(tablet-artist_info-Artistinfo)).

rule(tablet_06,
    event(spotify-info-Artistinfo) then
          action(pepper-say-Artistinfo)).

rule(tablet_07,
    intermediate(user-clicked-skip) then
        action(pino-skip-song)).

rule(tablet_08,
    event(tablet-clicked-example_start_button) then
        action(pepper-start_example-example)).

%%-"Laten we beginnen met een voorbeeld. Ik ga een liedje spelen.")).

rule(tablet_09,
    event(spotify-end_example-Example) then
        action(pepper-say-"Markeer het vakje van het nummer als je het op je kaart hebt staan. ")).

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

%%
%% General pepper behavior
%%
rule(greet_when_coming_close,
       event(pepper-just_arrived-Person) and
       event(Person-entered-zone1)
   then
       action(pepper-say-"Hallo daar")).

%%
%% Play more rules
%%

%% rule(speech_01,
%%    event(pepper-detected-speech) and
%%    believe(quiz-state-game)then
%%       action(pino-play-more)).

%% rule(speech_02,
%%    intermediate(song-played-more) then
%%        action(pepper-say-"Shall I play more?")).
