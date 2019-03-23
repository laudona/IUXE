:- module(rules, [rule/2]).

:- op(1200,	xfy	, then).
:- op(1100,	xfy	, or).
:- op(1000, xfy , and).
:- op(900, fy , not).


rule(test_tule,
        true 
    then 
        believe(agent-has-started)
).

rule(no_persons_at_start_rule,
        event(agent-is-ready) 
    then 
        believe(agent-sees_persons-0)
).

rule(one_more_person_rule,
        event(agent-saw_person_entering-Person) and
        believe(agent-sees_persons-Number) and
        NextNumber is Number + 1
    then 
        believe(agent-sees_persons-NextNumber)
).

rule(one_less_person_rule,
        event(agent-saw_person_leaving-Person)
    and
        believe(agent-sees_persons-Number)
    and
        NextNumber is Number - 1
    then 
        believe(agent-sees_persons-NextNumber)
).

rule(is_present_rule,
        believe(agent-sees_persons-Number)
    and
        Number > 0
    then 
        itermediate(person-is-present)
).

rule(rule2, 
        true
    then
        believe(agent-has-started)
).


rule(play_next_rule,
        event(spotify-played-LastSongUri) and
        believe(LastSong-uri-LastSongUri) and
        believe(LastSong-play_index-LastSongIndex) and
        NextSongIndex is LastSongIndex + 1 and
        believe(Song-play_index-NextSongIndex) and
        believe(Song-uri-SongUri) and
    then 
        action(spotify-play-SongUri)).
