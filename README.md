# Purring Butter Pepper Agent

## Requirements

- Node JS 8+ :[https://nodejs.org/en/]
- Python 2.7: [https://www.python.org/download/releases/2.7/]
- SWI Prolog: [https://nodejs.org/en/]


## Installation

```bash
$ git clone https://github.com/a-swarm-of-ralf/purring-butter.git
$ cd purring-butter/server
$ npm install
$ cd ../pepper
$ pip install -r /path/to/requirements.txt
```

## Running

Running the agent requires running serveral processes simultanious like the server, rule engine and some interfaces such as the Pepper interface. The first process to start should be the server, but the order of the other processes does not matter, though it is recomended to start the rules engine last.

The server is responsible for relaying messages between engine and interfaces. It also contains some interfaces and the authentication system. To start the server run the following command where the config parameter points to your interface configuration and the ip-address refers to the servers own ip-address. This is required when the server needs to communicate to other services.

```bash
node server/index.js --config ../data/config.json --ip-address 192.168.100.132
```

The Pepper client is responseble to send events from Pepper sensors to the server and is capable of receiving actions for simple tasks. Starting the Pepper client requires either the address of a Pepper robot or the address of the server:

```bash
python main.py --pepper_ip 192.168.0.101 --server_ip 192.168.0.105
```

The rules engine is started by running the prolog `start.pl` with SWI Prolog. It requires parameters for the ontology file and the rules file. Starting the rules engine:

```bash
swipl -t start:go start.pl  \
    --rules ../data/rules.pl \ 
    --ontology ../data/ontology.ttl \
    --port 3001 \
    --ip-address localhost
```

## Writing rules

Rules are writen in a prolog file containing one or more rule/2 predicates. By default the rules engine looks for the rules.pl file in the data directory.

An simple example rules file could look like below:

```prolog
:- module(rules, [rule/2]).

:- op(1200,	xfy	, then).
:- op(1100,	xfy	, or).
:- op(1000, xfy , and).
:- op(900, fy , not).

rule(agent_starts,
        event(agent-is-ready)
    then
        action(pepper-say-"Hallo, laten we muziek maken.")
).
```

This file contains a single rule _agent_starts_. When the agent starts Pepper will say the line noted between the brackets. Just make sure the Pepper interface is connected when starting the rules engine.

The op clause make sure the rule can be written with then, and, or and not as you would expect. It is not really required in the rules file but then the rules would need to look like:

```prolog
rule(agent_starts, then(event(agent-is-ready), action(pepper-say-"Hallo, laten we muziek maken."))).
```

#### Rule Specification

The current full specification of the rule defintion is shown below. This may be extended in the future.

```prolog
rule(<rule-name>, <rule-body>).

rule-body ::= <condition> then <goal>

goal ::= action(<triple>) | believe(<triple>) | intermediate(<triple>)

condition ::= <condition> and <condition> | <condition> or <condition> | not <condition> |
    <expression> | <rdf-condition>
  
rdf-condition ::= event(<triple>) | believe(<triple>) | intermediate(<triple>)

triple ::= <subject:resource>-<predicate:resource>-<object:term>  

term ::= <literal> | <resource>

resource ::=  <iri> | <prefix>:<atom> | <atom> 

literal ::= <float> | <string> | <integer>

expression ::= <left> <operator> <right> 

operator ::=  is | = | > | < | >= | =<

```

Right hand side of expressions with the `is` operation can be almost any expression that is allowed in normal Prolog in the same place.

The `event` and `intermediate` conditions exist only during a single pass of the rules engine. In other words they are not remembered. This means a rule having several seperate events will never succeed.

```prolog
rule(agent_starts,
        event(pepper-sees-Person) and event(spotify-played-Song)
    then
        action(pepper-say-"Something")
).
```

The rule above rule will never trigger because the two events will never exist in the same pass of the engine.

## Ontology

The ontology is a turtle file ( https://en.wikipedia.org/wiki/Turtle_(syntax) ). It is loaded when the rules engine starts. Facts in the ontology can be accessed in rules with the `believe` condition. 

```prolog
rule(rule51,
       believe('1xEV45D5xMSXSHxs7X27Zj'-name-Name)
   then
       action(pepepr-say-Name)).
```

The believe `'1xEV45D5xMSXSHxs7X27Zj'-name-SongUri` condition from the rule above wil refer to the ontology below and `Name` wil be bound to `"Son Of A Preacher Man"` if that ontology is loaded. `iuxe` is the default prefix and can be ommited in this case.

```ttl
@prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix owl:   <http://www.w3.org/2002/07/owl#> .
@prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
@prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> .
@prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .

iuxe:1xEV45D5xMSXSHxs7X27Zj iuxe:id "1xEV45D5xMSXSHxs7X27Zj" .
iuxe:1xEV45D5xMSXSHxs7X27Zj iuxe:uri "spotify:track:1xEV45D5xMSXSHxs7X27Zj" . 
iuxe:1xEV45D5xMSXSHxs7X27Zj iuxe:type iuxe:track . 
iuxe:1xEV45D5xMSXSHxs7X27Zj iuxe:name "Son Of A Preacher Man" . 
iuxe:1xEV45D5xMSXSHxs7X27Zj iuxe:artist "Dusty Springfield" . 
iuxe:1xEV45D5xMSXSHxs7X27Zj iuxe:img "https://i.scdn.co/image/a642ba3cde5ea1dc5307397eeb1d16adee2acb64" . 
iuxe:1xEV45D5xMSXSHxs7X27Zj iuxe:preview_url "null" . 
iuxe:1xEV45D5xMSXSHxs7X27Zj iuxe:duration_ms "145946"^^<http://www.w3.org/2001/XMLSchema#integer> . 

iuxe:3mJYXkGxOS2CM5ayWqY3SM iuxe:id "3mJYXkGxOS2CM5ayWqY3SM" .
iuxe:3mJYXkGxOS2CM5ayWqY3SM iuxe:uri "spotify:track:3mJYXkGxOS2CM5ayWqY3SM" . 
iuxe:3mJYXkGxOS2CM5ayWqY3SM iuxe:type iuxe:track . 
iuxe:3mJYXkGxOS2CM5ayWqY3SM iuxe:name "Mr. Tambourine Man" . 
iuxe:3mJYXkGxOS2CM5ayWqY3SM iuxe:artist "The Byrds" . 
iuxe:3mJYXkGxOS2CM5ayWqY3SM iuxe:img "https://i.scdn.co/image/3b55fdc18ee8e077abd9539d2390d182e03cf46f" . 
iuxe:3mJYXkGxOS2CM5ayWqY3SM iuxe:preview_url "https://p.scdn.co/mp3-preview/d2da910a7c07c76aa4b9b2438b5717a43afc54c8?cid=3848d012f506457997ebde1cb526ebcf" . 
iuxe:3mJYXkGxOS2CM5ayWqY3SM iuxe:duration_ms "137746"^^<http://www.w3.org/2001/XMLSchema#integer> . 

iuxe:5cjXFtQAc2ZRyWuEFEG06v iuxe:id "5cjXFtQAc2ZRyWuEFEG06v" .
iuxe:5cjXFtQAc2ZRyWuEFEG06v iuxe:uri "spotify:track:5cjXFtQAc2ZRyWuEFEG06v" . 
iuxe:5cjXFtQAc2ZRyWuEFEG06v iuxe:type iuxe:track . 
iuxe:5cjXFtQAc2ZRyWuEFEG06v iuxe:name "I'm a Believer" . 
iuxe:5cjXFtQAc2ZRyWuEFEG06v iuxe:artist "The Monkees" . 
iuxe:5cjXFtQAc2ZRyWuEFEG06v iuxe:img "https://i.scdn.co/image/03b9c72684f277f974b15f706b8e9529eb743203" . 
iuxe:5cjXFtQAc2ZRyWuEFEG06v iuxe:preview_url "https://p.scdn.co/mp3-preview/21d3ddc0aea9b09592a7381d8f269a8c7c3641ec?cid=3848d012f506457997ebde1cb526ebcf" . 
iuxe:5cjXFtQAc2ZRyWuEFEG06v iuxe:duration_ms "166906"^^<http://www.w3.org/2001/XMLSchema#integer> . 

iuxe:39yWVJ9ENz4FB4v5mZffj4 iuxe:id "39yWVJ9ENz4FB4v5mZffj4" .
iuxe:39yWVJ9ENz4FB4v5mZffj4 iuxe:uri "spotify:track:39yWVJ9ENz4FB4v5mZffj4" . 
iuxe:39yWVJ9ENz4FB4v5mZffj4 iuxe:type iuxe:track . 
iuxe:39yWVJ9ENz4FB4v5mZffj4 iuxe:name "Oh, Pretty Woman" . 
iuxe:39yWVJ9ENz4FB4v5mZffj4 iuxe:artist "Roy Orbison" . 
iuxe:39yWVJ9ENz4FB4v5mZffj4 iuxe:img "https://i.scdn.co/image/093238fab6b68306e247f44926a948498466e8dc" . 
iuxe:39yWVJ9ENz4FB4v5mZffj4 iuxe:preview_url "https://p.scdn.co/mp3-preview/ac479094ec51549d440f6aef317a35152376e0d8?cid=3848d012f506457997ebde1cb526ebcf" . 
iuxe:39yWVJ9ENz4FB4v5mZffj4 iuxe:duration_ms "176213"^^<http://www.w3.org/2001/XMLSchema#integer> . 

iuxe:4gphxUgq0JSFv2BCLhNDiE iuxe:id "4gphxUgq0JSFv2BCLhNDiE" .
iuxe:4gphxUgq0JSFv2BCLhNDiE iuxe:uri "spotify:track:4gphxUgq0JSFv2BCLhNDiE" . 
iuxe:4gphxUgq0JSFv2BCLhNDiE iuxe:type iuxe:track . 
iuxe:4gphxUgq0JSFv2BCLhNDiE iuxe:name "Jailhouse Rock" . 
iuxe:4gphxUgq0JSFv2BCLhNDiE iuxe:artist "Elvis Presley" . 
iuxe:4gphxUgq0JSFv2BCLhNDiE iuxe:img "https://i.scdn.co/image/cc7323d63e79dd46fea998f99ef459544114b01c" . 
iuxe:4gphxUgq0JSFv2BCLhNDiE iuxe:preview_url "https://p.scdn.co/mp3-preview/29990f669b5328b6c40320596a2b14d8660cdb54?cid=3848d012f506457997ebde1cb526ebcf" . 
iuxe:4gphxUgq0JSFv2BCLhNDiE iuxe:duration_ms "146480"^^<http://www.w3.org/2001/XMLSchema#integer> . 

iuxe:playlist iuxe:starts_with iuxe:1xEV45D5xMSXSHxs7X27Zj .
iuxe:1xEV45D5xMSXSHxs7X27Zj iuxe:is_followed_by iuxe:3mJYXkGxOS2CM5ayWqY3SM .
iuxe:3mJYXkGxOS2CM5ayWqY3SM iuxe:is_followed_by iuxe:5cjXFtQAc2ZRyWuEFEG06v .
iuxe:5cjXFtQAc2ZRyWuEFEG06v iuxe:is_followed_by iuxe:39yWVJ9ENz4FB4v5mZffj4 .

```

Example of two rules that can play the song in sequence (if the `event(spotify-played-LastSongUri)` event works...).

```prolog
:- module(rules, [rule/2]).

:- op(1200,	xfy	, then).
:- op(1100,	xfy	, or).
:- op(1000, xfy , and).
:- op(900, fy , not).

rule(play_first_song,
       event(agent-is-ready) and
       believe(playlist-starts_with-Song) and
       believe(Song-uri-SongUri)
   then
       action(spotify-play-SongUri)).

rule(play_next_rule,
       event(spotify-played-LastSongUri) and
       believe(LastSong-uri-LastSongUri) and
       believe(LastSong-is_followed_by-Song) and
       believe(Song-uri-SongUri)
   then
       action(spotify-play-SongUri)).

```

## Interfaces

Most interfaces will define serveral events and actions.

### Timer

The timer interface allows for the ability to trigger events at certain times. The interface is based on the javascript setTimeout and setInterface functions.


The timer interface defines four actions:

* timer set_timeout `<timeout-in-ms>/<labe>` 
* timer set_interval `<interval-in-ms>/<label>`
* timer clear_interval `<label>`
* timer clear_timeout `<label>` 

A `<timeout-in-ms>/<labe>` is a string consisting of a integer for the tmeout and a string label used
in the events. This should be formated in action like shown below:

```
action(timer-set_timeout-"5000/stop_music")
```

The four actions can trigger three events:

* timer set `<label>` 
* timer triggered `<label>` 
* timer cleared `<label>` 

The label will always be a string.

the example below show how a timer can be set and reacted upon. Interval work the same
except `set_timeout` becomes `set_interval`.

```prolog
rule(rule1,
        event(agent-is-ready)
    then
        action(time-set_timeout-"10000/wait_10")
).

rule(rule3,
        event(timer-set-"wait_10")
    then
        action(pepper-say-"I have set a timeout for 10 seconds.")
).

rule(rule3,
        event(timer-triggered-"wait_10")
    then
        action(pepper-say-"The time is now!")
).
```

### Pepper

```prolog
action(pepper-say-”Hello, how are you.”).
action(pepper-listen-”Yes, No, Maybe”).
action(pepper-play-”~/music/evil_laugh.ogg”).

event(pepper-heard-WordId) and
event(WordId-word-Word) and
event(WordId-heard-Confidence)

event(pepper-was_touched-rightBumper)
event(pepper-was_touched-headFront)

action(pepper-navigate_to-”1.0, 2.0”).
action(pepper-move_to-”0.0, 0.0, 1.0702”).
action(pepper-run-”/animation/Stand/Hey_01”).
action(pepper-start-”/animation/Stand/Hey_01”).
action(pepper-show-”https://html5test.com/”).
```

```ttl
@prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
@prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
      
iuxe:pepper iuxe:just_arrived iuxe:person_90562 .
iuxe:person_90562 iuxe:distance "1.36600005627"^^xsd:decimal .
iuxe:person_90562 iuxe:face_detected "0"^^xsd:boolean .
iuxe:person_90562 iuxe:is_visible "1"^^xsd:decimal .
iuxe:person_90562 iuxe:not_seen_since "0"^^xsd:integer .
iuxe:person_90562 iuxe:present_since "4"^^xsd:integer .
iuxe:person_90562 iuxe:height "1.6529815197"^^xsd:decimal .
iuxe:person_90562 iuxe:shirt_color "Black" .
```

```prolog
rule(complain_when_touched,
       event(pepper-was_touched-front_head) or
       event(pepper-was_touched-middle_head) or 
		event(pepper-was_touched-back_head)
   then
       action(pepper-say-”Hey, that tickles!”)).
```

### Spotify

To start with the spotify interface some preparations are required.

- Create a (free) spotify account at www.spotify.com
- Go to developer dashboard at https://developer.spotify.com/dashboard/applications and create an application.
- In the settings add ‘http://localhost:3000/spotify/callback’ to under Redirect URIs.
- In the configuration of the agent note the clientId and clientSecret from your spotify application.

When starting the interface we need to login to spotify to enable access and playback.

- Start the server
- Go to http://localhost:3000/public/spotify/login.html and click the link.
- Login with any spotify account, though playback requires a premium account.

The following actions and events are defined:

```prolog
action(spotify-play-"spotify:track:5cjXFtQAc2ZRyWuEFEG06v"). %% Start action(spotify-play-current). %% Resume a User's Playback
action(spotify-play-next). %% Skip User’s Playback To Next Track
action(spotify-play-previous). %%  Skip User’s Playback To Previous Track
actions(action(spotify-seek-5000). %%  Seek To Position in msec
actions(action(spotify-playback-pause). %% Pause a User's Playback
actions(action(spotify-playback_on-"device_uri")). %% Transfer Playback

event(spotify-played-SongUri).
event(spotify-playing-SongUri).
event(spotify-activated_device-DeviceUri).
```

##### Search

At http://localhost:3000/public/spotify/search.html this is a simple search page to look up songs on spotify and to see a suggested set of triples for a song. 


### Mock Event Generator

The Mock Event Generator can be used to simulate events and actions. The simulator should run on the server at http://localhost:3000/public/simulator/index.html .