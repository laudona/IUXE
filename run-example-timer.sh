#!/usr/bin/env bash
cd engine
swipl -t start:go start.pl --rules ../examples/timer/rules.pl --ontology ../examples/timer/ontology.ttl --port 3001 --ip-address localhost