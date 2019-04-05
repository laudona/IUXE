:- module(ontology_reader, []).

:- use_module(library(debug)).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf11)).


%%
%% Load ontology from specified file.
%%
load_ontology_file(Graph, File) :-
    debug(debug/ontology_reader/load_ontology_file, 'loading ontology \'~w\'...', [File]),
    open(File, read, Stream),
    debug(debug/ontology_reader/load_ontology_file, '~w file opened', [File]),
    call_cleanup(rdf_read_turtle(stream(Stream), Triples, [ prefixes(Prefixes) ]), close(Stream)),
    debug(debug/ontology_reader/load_ontology_file, '~w file closed.', [File]),
    load_prefixes(Prefixes),
    load_triples(Graph, Triples).

%%
%% Load ontology from a string.
%%
load_ontology_string(Graph, String) :-
    debug(info, 'loading ontology from string...', [String]),
    open_string(String, Stream),
    call_cleanup(rdf_read_turtle(stream(Stream), Triples, [ prefixes(Prefixes) ]), close(Stream)),
    load_prefixes(Prefixes),
    load_triples(Graph, Triples).

%%
%% Presist prefixes
%%
load_prefixes([]).
load_prefixes([ Prefix-Uri | Rest ]) :-
    debug(info, 'loading prefix \'~w\' - <~w>...', [Prefix, Uri]),
    rdf_register_prefix(Prefix, Uri, [keep(true)]),
    load_prefixes(Rest).

%%
%% Persist the triples to Graph.
%%
load_triples(_Graph, []).
load_triples(Graph, [ rdf(S, P, literal(type(TypeIRI, O))) | Rest ]) :-
    !,
    debug(load_triples/info, 'loading triple \'~w\'...', [S]),
    debug(load_triples/info, '               \'~w\'...', [P]),
    debug(load_triples/info, '               literal \'~w\' of type <~w>...', [O, TypeIRI]),
    rdf_assert(S, P, O^^TypeIRI, Graph),
    load_triples(Graph, Rest).
load_triples(Graph, [ rdf(S, P, literal(O)) | Rest ]) :-
    !,
    debug(load_triples/info, 'loading triple \'~w\'...', [S]),
    debug(load_triples/info, '               \'~w\'...', [P]),
    debug(load_triples/info, '               literal \'~w\'...', [O]),
    rdf_assert(S, P, O^^'http://www.w3.org/2001/XMLSchema#string', Graph),
    load_triples(Graph, Rest).
load_triples(Graph, [ rdf(S, P, O) | Rest ]) :-
    !,
    debug(load_triples/info, 'loading triple \'~w\'...', [S]),
    debug(load_triples/info, '               \'~w\'...', [P]),
    debug(load_triples/info, '               \'~w\'...', [O]),
    rdf_assert(S, P, O, Graph),
    load_triples(Graph, Rest).
load_triples(Graph, [ rdf(S, P, O, G ) | Rest ]) :-
    !,
    debug(load_triples/info, 'loading quad as triple discarding graph \'~w\'...', [G]),
    load_triples(Graph, [ rdf(S, P, O) | Rest ]).

%%
%% Reads a triple from the database
%%
read_rdf(G, S, P, Value) :- 
    rdf(S, P, Object, G),
    Object = ^^(Value, Type),
    debug(ontology_reader/info, '[ONTOLOGY] Read rdf literal \'~w\' of typen <~w>.', [Value, Type]),
    debug(ontology_reader/info, 'read rdf \'~w\' - \'~w\' - \'~w\'.', [S, P, Value]).
read_rdf(G, S, P, Value) :- 
    rdf(S, P, Object, G),
    Object = @(Value, Lang),
    debug(ontology_reader/info, '[ONTOLOGY] Read rdf string \'~w\' of language <~w>.', [Value, Lang]),
    debug(ontology_reader/info, 'read rdf \'~w\' - \'~w\' - \'~w\'.', [S, P, Value]).
read_rdf(G, S, P, Value) :- 
    rdf(S, P, Object, G),
    Object \= ^^(_Value1, _Type),
    Object \= @(_Value2, _Lang),
    Object = Value,
    debug(ontology_reader/info, '[ONTOLOGY] Read rdf which was not a literal so probably an iri? - <~w>.', [Object]),
    debug(ontology_reader/info, 'read rdf \'~w\' - \'~w\' - \'~w\'.', [S, P, Value]).

%%
%% Writes a triple to the database
%%
write_rdf(G, S, P, ^^(Value, _Type)) :-
    debug(ontology_reader/info, 'writing believe \'~w\' with \'~w\'...', [S, Value]),
    read_rdf(G, S, P, Value),
    debug(ontology_reader/info, 'skipped believe \'~w\' because it already existed.', [P]).
write_rdf(G, S, P, ^^(Value, _Type)) :-
    debug(ontology_reader/info, 'writing believe \'~w\' - \'~w\' - \'~w\'...', [S, P, Value]),
    rdf_assert(S, P, Value, G),
    debug(ontology_reader/info, 'wrote believe \'~w\' - \'~w\' - \'~w\'.', [S, P, Value]).
write_rdf(G, S, P, O) :-
    debug(ontology_reader/info, 'writing believe \'~w\' - \'~w\' - \'~w\'...', [S, P, O]),
    read_rdf(G, S, P, O),
    debug(ontology_reader/info, 'skipped believe \'~w\' because it already existed.', [S, P, O]).
write_rdf(G, S, P, O) :-
    debug(ontology_reader/info, 'writing believe \'~w\' - \'~w\' - \'~w\'...', [S, P, O]),
    rdf_assert(S, P, O, G),
    debug(ontology_reader/info, 'wrote believe \'~w\' - \'~w\' - \'~w\'.', [S, P, O]).

%%
%% Writes a triple to the database
%%
delete_rdf(G, S, P, O) :- 
    debug(debug/ontology_reader/delete_rdf, 'retracting rdf \'~w\' - \'~w\' - \'~w\' from ~w...', [S, P, O, G]),
    rdf_retractall(S, P, O, G),
    debug(debug/ontology_reader/delete_rdf, 'retracted rdf \'~w\' - \'~w\' - \'~w\'.', [S, P, O]).
delete_rdf(G, S, P, O) :- 
    debug(debug/ontology_reader/delete_rdf, 'retracting literal rdf \'~w\' - \'~w\' - \'~w\'...', [S, P, O]),
    rdf_retractall(S, P, ^^(O, _Type), G),
    debug(debug/ontology_reader/delete_rdf, 'retracted rdf \'~w\' - \'~w\' - \'~w\'.', [S, P, O]).
delete_rdf(G, S, P, O) :- 
    debug(debug/ontology_reader/delete_rdf, 'retracting literal string rdf \'~w\' - \'~w\' - \'~w\'...', [S, P, O]),
    rdf_retractall(S, P, @(O, _Lang), G),
    debug(debug/ontology_reader/delete_rdf, 'retracted rdf \'~w\' - \'~w\' - \'~w\'.', [S, P, O]).


%%
%% Reads a triple from the database from the relevant graph
%%
read_believe(S, P, O) :- read_rdf(believe, S, P, O).
read_event(S, P, O) :- read_rdf(event, S, P, O).
read_action(S, P, O) :- read_rdf(action, S, P, O).
read_intermediate(S, P, O) :- read_rdf(intermediate, S, P, O).

%%
%% Writes a triple to the database in the relevant graph
%%
write_believe(S, P, O) :- write_rdf(believe, S, P, O).
write_event(S, P, O) :- write_rdf(event, S, P, O).
write_action(S, P, O) :- write_rdf(action, S, P, O).
write_intermediate(S, P, O) :- write_rdf(intermediate, S, P, O).

write_unbelieves([]).
write_unbelieves([ unbelieve(S, P, O) | Unbelieves ]) :- 
    delete_rdf(believe, S, P, O),
    write_unbelieves(Unbelieves).
write_unbelieves([ _U | Unbelieves ]) :- 
    write_unbelieves(Unbelieves).

write_believes([]).
write_believes([ believe(S, P, O) | Believes ]) :- 
    debug(ontology_reader/info, 'Writing believe about \'~w\' ...', [S]),
    write_rdf(believe, S, P, O),
    write_believes(Believes).
write_believes([ believe(S, _P, _O) | Believes ]) :- 
    debug(ontology_reader/info, 'Skipping believe about \'~w\' ...', [S]),
    write_believes(Believes).

write_actions([]).
write_actions([ action(S, P, O) | Actions ]) :- 
    write_rdf(action, S, P, O),
    write_actions(Actions).
write_actions([ _Action | Actions ]) :- 
    write_actions(Actions).

%%
%% Deletes all triples of teh specified graph.
%%
reset_graph(Graph) :-
    delete_rdf(Graph, _S, _P, _O).

%%
%% Tests
%%
:- begin_tests(ontology_reader).

test(load_ontology_1, [ nondet, cleanup(reset_graph(believe))]) :-
    debug(ontology_reader/load/info),
    ontology_reader:load_ontology_file(believe, '../data/ontology.ttl').    

:- end_tests(ontology_reader).