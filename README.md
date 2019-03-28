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

Starting the server:

```bash
node server/index.js --config ../data/config.json --ip-address 192.168.100.132
```

Starting the rules engine:

```bash
swipl -t start:go start.pl  \
    --rules ../data/rules.pl \ 
    --ontology ../data/ontology.ttl \
    --port 3001 \
    --ip-address localhostColocolo:purring-butter
```

Starting the Pepper client:

```bash
python main.py --pepper_ip 192.168.0.101 --server_ip 192.168.0.105
```

## Writing rules

Rules are writen in the rules.pl file in the data directory. An example rules.pl file looks like:

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

The Mock Event Generator can be used to simulate events and actions. The simulator should run on