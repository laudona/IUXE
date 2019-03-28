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


```ttl
@prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
@prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .

iuxe:5cjXFtQAc2ZRyWuEFEG06v iuxe:uri "spotify:track:5cjXFt..." .
iuxe:5cjXFtQAc2ZRyWuEFEG06v iuxe:name "I'm a Believer" .
iuxe:5cjXFtQAc2ZRyWuEFEG06v iuxe:artist "The Monkees" .
iuxe:5cjXFtQAc2ZRyWuEFEG06v iuxe:play_index "1"^^xsd:integer .

iuxe:5cjXFtQAc2ZRyWuEFEG06v iuxe:uri "spotify:track:39yWVJ9ENz4FB4v..." .
iuxe:5cjXFtQAc2ZRyWuEFEG06v iuxe:name "Oh, Pretty Woman" .
iuxe:5cjXFtQAc2ZRyWuEFEG06v iuxe:artist "Roy Orbison" .
iuxe:5cjXFtQAc2ZRyWuEFEG06v iuxe:play_index "2"^^xsd:integer .

iuxe:playlist iuxe:starts_with iuxe:1xEV45D5xMSXSHxs7X27Zj .
iuxe:1xEV45D5xMSXSHxs7X27Zj iuxe:is_followed_by iuxe:3mJYXkGxOS2CM5ayWqY3SM .
iuxe:3mJYXkGxOS2CM5ayWqY3SM iuxe:is_followed_by iuxe:5cjXFtQAc2ZRyWuEFEG06v .
iuxe:5cjXFtQAc2ZRyWuEFEG06v iuxe:is_followed_by iuxe:39yWVJ9ENz4FB4v5mZffj4 .

```

## Interfaces

Most interfaces will define serveral events and actions.

### Timer

The timer interface allows for the ability to trigger events at certain times. The interface is based on the javascript setTimeout and setInterface functions.

The timer interface defines four actions:

* timer set_timeout `<timeout-in-ms>` 
* timer set_interval `<interval-in-ms>`
* timer clear_interval `<id>`
* timer clear_timeout `<id>` 

The actions can trigger six events:

* timer timeout_set `<id>` 
* timer timeout_triggered `<id>` 
* timer timeout_cleared `<id>` 
* timer interval_set `<id>` 
* timer interval_triggered `<id>` 
* timer interval_cleared `<id>` 

Example
```prolog
rule(rule1,
        event(agent-is-ready)
    then
        action(time-timeout_set-10000)
).

rule(rule3,
        event(timer-interval_set-Id)
    then
        action(pepper-say-"I have set a timeout for 10 seconds.")
).

rule(rule3,
        event(timer-timeout_triggered-Id)
    then
        action(pepper-say-"The time is now!")
).
```

### Pepper

TODO

### Spotify

TODO

### Mock Event Generator

The Mock Event Generator can be used to simulate events and actions. The simulator should run on the server at http://localhost:3000/public/simulator/index.html .