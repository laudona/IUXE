#!/usr/bin/env bash
cd engine
swipl -t start:go start.pl --rules ../examples/music-quiz/rules.pl --ontology ../examples/music-quiz/ontology.ttl --port 3001 --ip-address localhost