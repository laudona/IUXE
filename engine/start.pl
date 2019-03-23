:- module(start, []).

:- use_module(library(debug)).

:- use_module(argparse, []).
:- use_module(ontology_reader, []).


go :-
    debug(info),
    argparse:args,
    argparse:arg(ontology, OntologyFile),
    ontology_reader:load_ontology_file(OntologyFile).

