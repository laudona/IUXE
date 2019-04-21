#!/usr/bin/env bash
cd engine
swipl -t start:go start.pl --rules ../examples/counter/rules.pl --ontology ../examples/counter/ontology.ttl --port 3001 --ip-address localhost