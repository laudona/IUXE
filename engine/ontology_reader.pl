:- module(ontology_reader, []).

:- use_module(library(debug)).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf11)).

%%
%% Load ontology from specified file.
%%
load_ontology_file(File) :-
    debug(ontology_reader/load/info, 'loading ontology ~w...', [File]),
    open(File, read, Stream),
    debug(ontology_reader/load/info, '~w file opened', [File]),
    call_cleanup(rdf_read_turtle(stream(Stream), Triples, [ prefixes(Prefixes) ]), close(Stream)),
    debug(ontology_reader/load/info, '~w file closed.', [File]),
    debug(ontology_reader/load/info, 'prefixes loaded: ~w', [Prefixes]),
    debug(ontology_reader/load/info, 'triples loaded: ~w', [Triples]).

%%
%% Tests
%%
:- begin_tests(ontology_reader).

test(load_ontology_1, []) :-
    debug(ontology_reader/load/info),
    load_ontology_file('../data/ontology.ttl').    

:- end_tests(ontology_reader).