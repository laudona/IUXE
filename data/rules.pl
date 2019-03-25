:- module(rules, [rule/2]).

:- op(1200,	xfy	, then).
:- op(1100,	xfy	, or).
:- op(1000, xfy , and).
:- op(900, fy , not).

rule(agent_starts,
        event(agent-is-ready)
    then
        believe(agent-intent-wait_for_next_person)
).

rule(person_comes_close,
        believe(agent-intent-wait_for_next_person) and
        event(pepper-sees-Person) and
        event(Person-entered-zone1)
    then
        intermediate(agent-greets-person)        
).

rule(person_comes_close1,
        intermediate(agent-greets-person)
    then
        believe(agent-greeted-person)
).

rule(person_comes_close2,
        intermediate(agent-greets-person)
    then
        action(pepper-say-"Hallo, zullen we wat muziek luisteren?")
).

rule(person_comes_close3,
        intermediate(agent-greets-person)
    then
        action(pepper-listen-"Ja, Nee, Misschien")
).

rule(person_answered_maybe,
        believe(agent-intent-asked_music) and
        event(pepper-heard-WordId) and
        event(WordID-word-"Misschien")
    then
        action(pepper-say-"Er is geen haast, denk rustig na.")
).

rule(person_answered_no,
        believe(agent-intent-asked_music) and
        event(pepper-heard-WordId) and
        event(WordID-word-"Nee")
    then
        intermediate(agent-ends-playback)
).

rule(agent_stops_playlist1,
        intermediate(agent-starts-playback)
    then
        action(pepper-listen-"")
).

rule(agent_stops_playlist2,
        intermediate(agent-starts-playback)
    then
        action(pepper-say-"Oh, dat is jammer.")
).

rule(agent_stops_playlist3,
        intermediate(agent-starts-playback)
    then
        action(spotify-playback-pause)
).

rule(agent_starts_playlist,
        believe(agent-greeted-person) and
        event(pepper-heard-WordId) and
        event(WordID-word-"Ja")
    then
        intermediate(agent-starts-playback)
).

rule(agent_starts_playlist1,
        intermediate(agent-starts-playback)
    then
        action(pepper-listen-"")
).

rule(agent_starts_playlist2,
        intermediate(agent-starts-playback)
    then
        action(pepper-say-"Geweldig, dan ga ik nu het eerste liedje spelen")
).

rule(agent_starts_playlist3,
        intermediate(agent-starts-playback) and
        believe(playlist-starts_with-Song) and
        believe(Song-uri-Uri)
    then
        action(spotify-play-Uri)
).

rule(song_finished,
        event(spotify-played-SongUri) and
        believe(Song-uri-Uri) and
        believe(Song-is_followed_by-NextSong)
    then
        intermediate(agent-plays-next_song)
).

rule(play_next_song1,
        intermediate(agent-plays-next_song) and
        believe(Song-uri-Uri) and
        believe(Song-is_followed_by-NextSong)
    then
        action(spotify-play-NextSong)
).

rule(play_next_song2,
        intermediate(agent-plays-next_song)
    then
        action(pepper-say-"En nu het volgende liedje.")
).

