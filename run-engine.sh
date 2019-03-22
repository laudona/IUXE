#!/usr/bin/env bash
cd engine
swipl -t start:go start.pl --rules ../data/private/rules.pl --ontology ../data/private/ontology.ttl --port 3001 --ip-address localhost