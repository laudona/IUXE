#!/usr/bin/env bash
cd engine
swipl -t start:go start.pl --rules ../engine/music_quiz_pepper/rules_pepper.pl --ontology ../engine/music_quiz_pepper/ontology_pepper.ttl --port 3001 --ip-address localhost