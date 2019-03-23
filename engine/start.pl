:- module(start, []).

:- use_module(library(debug)).

:- use_module(argparse, []).
:- use_module(ontology_reader, []).
:- use_module(library(settings)).


go :-
    debug(info),
    debug(info, 'parsing arguments...', []),
    argparse:args,
    debug(info, 'reading ontology...', []),
    setting(argparse:ontology, OntologyFile),
    ontology_reader:load_ontology_file(believes, OntologyFile),
    debug(info, 'reading rules...', []).


%%
%% Tests
%%
:- begin_tests(start).

test(load_ontology_1, []) :-
    debug(info),
    ontology_reader:load_ontology_file(believes, '../data/ontology.ttl'),
    ontology_reader:read_rdf('http://www.tudelft.nl/ewi/iuxe#5cjXFtQAc2ZRyWuEFEG06v', 'http://www.tudelft.nl/ewi/iuxe#name', Name),
    debug(info, 'reading name \'~w\'...', [Name]),
    ontology_reader:read_rdf(WordId, 'http://www.tudelft.nl/ewi/iuxe#confidence', ^^(Confidence, Type)),
    Confidence2 is Confidence + 1,
    debug(info, 'reading the confidence of \'~w\' is ~w...', [WordId, Confidence2]).   

:- end_tests(start).

