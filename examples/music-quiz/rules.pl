:- module(rules, []).

:- op(1200,	xfy	, then).
:- op(1100,	xfy	, or).
:- op(1000, xfy , and).
:- op(900, fy , not).

rule(ready_01, event(agent-is-ready) then  action(pepper-listen-"A, B, C, Ja, Nee, Beginnen, Stoppen, Eenvoudig, Normaal, Moeilijk")).
rule(ready_02, event(agent-is-ready) then  intermediate(quiz-start-waiting)).


%%
%% Chain states
%%

rule(state_01, intermediate(setup-is-finished) and believe(quiz-current-Topic) then intermediate(quiz-play_fragment-Topic)).
rule(state_01, intermediate(play_fragment-is-finished) and believe(quiz-current-Topic) then intermediate(quiz-ask_topic-Topic)).
rule(state_01, intermediate(ask_topic-is-finished) and believe(quiz-current-Topic) then intermediate(quiz-end_topic-Topic)).
rule(state_01, intermediate(end_topic-is-finished) and believe(quiz-current-Topic) then intermediate(quiz-finished-Topic)).

%%
%% Chain topics when previous topic finsihed.
%%
rule(starting_01, intermediate(quiz-finished-Topic) and believe(Topic-is_followed_by-OtherTopic) then intermediate(quiz-setup-OtherTopic)).
rule(starting_02, intermediate(quiz-start-Topic) then intermediate(quiz-setup-Topic)).

%%
%% Handle pepper word inputs
%%
rule(p1, event(pepper-heard-Wid) and event(Wid-word-"A") then intermediate(user-said-a)).
rule(p2, event(pepper-heard-Wid) and event(Wid-word-"B") then intermediate(user-said-b)).
rule(p3, event(pepper-heard-Wid) and event(Wid-word-"C") then intermediate(user-said-c)).
%% rule(p4, event(pepper-heard-Wid) and event(Wid-word-"Ja") then intermediate(user-said-yes)).
%% rule(p5, event(pepper-heard-Wid) and event(Wid-word-"Nee") then intermediate(user-said-no)).
rule(p6, event(pepper-heard-Wid) and event(Wid-word-"Beginnen") then intermediate(user-said-start)).
%% rule(p7, event(pepper-heard-Wid) and event(Wid-word-"Stoppen") then intermediate(user-said-stop)).
rule(p8, event(pepper-heard-Wid) and event(Wid-word-"Eenvoudig") then intermediate(user-said-eenvoudig)).
rule(p9, event(pepper-heard-Wid) and event(Wid-word-"Normaal") then intermediate(user-said-normaal)).
rule(p10, event(pepper-heard-Wid) and event(Wid-word-"Moeilijk") then intermediate(user-said-moeilijk)).
rule(p10, event(pepper-said-Word) then intermediate(pepper-said-word)).

%%
%% Handle tablet inputs
%%
rule(t1, event(tablet-clicked-button_a) and believe(quiz-current-ask_topic) then intermediate(user-said-a)).
rule(t2, event(tablet-clicked-button_b) and believe(quiz-current-ask_topic)  then intermediate(user-said-b)).
rule(t3, event(tablet-clicked-button_c) and believe(quiz-current-ask_topic)  then intermediate(user-said-c)).
%% rule(t4, event(tablet-clicked-button_a) then intermediate(user-said-yes)).
%% rule(t5, event(tablet-clicked-button_b) then intermediate(user-said-no)).
rule(t6, event(tablet-clicked-button_a) and believe(quiz-current-waiting) then intermediate(user-said-start)).
%% rule(t7, event(tablet-clicked-button_a) then intermediate(user-said-stop)).
rule(t8, event(tablet-clicked-button_c) and believe(quiz-state-moeilijkheid)  then intermediate(user-said-eenvoudig)).
rule(t9, event(tablet-clicked-button_b) and believe(quiz-state-moeilijkheid)  then intermediate(user-said-normaal)).
rule(t10, event(tablet-clicked-button_a) and believe(quiz-state-moeilijkheid)  then intermediate(user-said-moeilijk)).

%%
%% Just for testing
%%
rule(test01, intermediate(user-said-X) then action(tablet-user_said-X)).


%%
%% Handle timer inputs
%%
rule(ti1, event(timer-trigger-"fragment_stop") and believe(quiz-current-playing_fragment) then intermediate(quiz-played-fragment)).
rule(ti2, event(timer-trigger-"full_stop") and believe(quiz-current-playing_fragment) then intermediate(quiz-played-full)).

%%
%% Setup a question or information
%%
rule(setup_01, intermediate(quiz-setup-Topic) and believe(Topic-title-Text) then action(tablet-title-Text)).
rule(setup_02, intermediate(quiz-setup-Topic) and believe(Topic-main-Text) then action(tablet-main-Text)).
rule(setup_03, intermediate(quiz-setup-Topic) and believe(Topic-support-Text) then action(tablet-support-Text)).
rule(setup_04, intermediate(quiz-setup-Topic) and believe(Topic-button_a_text-Text) then action(tablet-button_a_text-Text)).
rule(setup_05, intermediate(quiz-setup-Topic) and believe(Topic-button_b_text-Text) then action(tablet-button_b_text-Text)).
rule(setup_06, intermediate(quiz-setup-Topic) and believe(Topic-button_c_text-Text) then action(tablet-button_c_text-Text)).
rule(setup_07, intermediate(quiz-setup-Topic) and believe(Topic-button_a_text-Text) then action(tablet-show-button_a_text)).
rule(setup_08, intermediate(quiz-setup-Topic) and believe(Topic-button_b_text-Text) then action(tablet-show-button_b_text)).
rule(setup_09, intermediate(quiz-setup-Topic) and believe(Topic-button_c_text-Text) then action(tablet-show-button_c_text)).
rule(setup_10, intermediate(quiz-setup-Topic) and not believe(Topic-button_a_text-Text) then action(tablet-hide-button_a_text)).
rule(setup_11, intermediate(quiz-setup-Topic) and not believe(Topic-button_b_text-Text) then action(tablet-hide-button_b_text)).
rule(setup_12, intermediate(quiz-setup-Topic) and not believe(Topic-button_c_text-Text) then action(tablet-hide-button_c_text)).
rule(setup_13, intermediate(quiz-setup-Topic) and believe(Topic-say_during_setup-Text) then action(pepper-say-Text)).
rule(setup_14, intermediate(quiz-setup-Topic) then believe(quiz-current-Topic)).
rule(setup_15, intermediate(quiz-setup-Topic) then believe(quiz-state-setup)).
rule(setup_16, intermediate(quiz-setup-Topic) then unbelieve(quiz-current-OtherTopic)).
rule(setup_17, intermediate(quiz-setup-Topic) then unbelieve(quiz-state-OtherState)).
rule(setup_answered, intermediate(quiz-setup-Topic) then unbelieve(user-answered-topic)).
rule(setup_18, intermediate(quiz-setup-Topic) and believe(Topic-show_page-PageUrl) then action(pepper-show-PageUrl)).

%%
%% Finish setup
%%
rule(setup_19, intermediate(pepper-said-word) and believe(quiz-state-setup) then intermediate(setup-is-finished)).

%%
%% Play a fragment for a question or information
%%
rule(fragment_01, intermediate(quiz-play_fragment-Topic) and believe(Topic-song_uri-Uri) then action(spotify-play-Uri)).
rule(fragment_02, intermediate(quiz-play_fragment-Topic) and believe(moeilijkheid-current-T) then action(timer-set_timeout-T)).
rule(fragment_03, intermediate(quiz-play_fragment-Topic) then believe(quiz-state-play_fragment)).
rule(fragment_04, intermediate(quiz-play_fragment-Topic) then unbelieve(quiz-state-Oth)).

%%
%% Finshished playing fragment
%%
rule(fragment_05, intermediate(quiz-played-fragment) then action(spotify-playback-pause)) .
rule(fragment_06, intermediate(quiz-played-fragment) then intermediate(play_fragment-is-finished)).

%%
%% Ask the main question of the topic
%%
rule(ask_topic_01, intermediate(quiz-ask_topic-Topic) and believe(Topic-main-Text) then action(pepper-say-Text)).
rule(ask_topic_03, intermediate(quiz-ask_topic-Topic) then believe(quiz-state-ask_topic)).
rule(ask_topic_04, intermediate(quiz-ask_topic-Topic) then unbelieve(quiz-state-Oth)).

%%
%% Finshished asking and answering
%%
rule(ask_topic_05, 
    intermediate(user-said-Answer) and 
    believe(quiz-state-ask_topic) and 
    believe(quiz-current-Topic) and 
    believe(Topic-answer-Answer) and  
    believe(Topic-type-Type) then 
        intermediate(ask_topic-Type-Answer)) .

rule(ask_topic_06, intermediate(ask_topic-question-Answer) and believe(quiz-current-Topic) and believe(Topic-correct-Answer) then intermediate(Topic-answered-correct)) .
rule(ask_topic_07, intermediate(ask_topic-question-Answer) and believe(quiz-current-Topic) and not believe(Topic-correct-Answer) then intermediate(Topic-answered-incorrect)) .

rule(ask_topic_08, intermediate(ask_topic-waiting-Answer) then action(pepper-say-"OK, laten we beginnen.")) .
rule(ask_topic_09, intermediate(ask_topic-moeilijkheid-Answer) then action(pepper-say-"Dat is goed. We gaan verder met deze moeilijkheidgraad.")) .
rule(ask_topic_10, intermediate(ask_topic-score-Answer) then action(pepper-say-"Dat was het. Ik hoop dat je het leuk vond.")) .

rule(ask_topic_11, intermediate(user-answered-incorrect) then action(pepper-say-"Sorry, dat was niet goed. Probeer het nog een keer.")) .
rule(ask_topic_12, intermediate(user-answered-correct) then action(pepper-say-"Ja, dat was het goede antwoord.")) .

rule(ask_topic_101, intermediate(ask_topic-Type-Answer) then believe(user-answered-topic)) .
rule(ask_topic_102, intermediate(pepper-said-word) and believe(user-answered-topic) then intermediate(ask_topic-is-finished)) .


%%
%% Finishing a question or information
%%
rule(end_topic_01, intermediate(quiz-end_topic-Topic) then action(pepper-say-"Laten we door gaan.")).
rule(end_topic_02, intermediate(quiz-end_topic-Topic) then believe(quiz-state-end_topic)).
rule(end_topic_03, intermediate(quiz-end_topic-Topic) then unbelieve(quiz-state-Oth)).

%%
%% Finish a question or information
%%
rule(end_topic_04, intermediate(pepper-said-word) and believe(quiz-state-end_topic) then intermediate(end_topic-is-finished)).


