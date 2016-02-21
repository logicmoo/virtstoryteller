% Only executable if load.pl was loaded
:- ensure_loaded('../../prolog/load.pl').

% Define Namespaces for domain
:- rdf_db:rdf_register_ns(psj2, 'http://www.owl-ontologies.com/StoryWorldSettings/Pirates_Jasper2.owl#').
:- rdf_db:rdf_load('ontology/Pirates_Jasper2.owl').
:- rdf_db:rdf_load('setting/ship.ttl').
:- rdf_db:rdf_load('setting/Jean-Baptiste.ttl').

% Load schemas into schema management module
:- consult(schemas:'schemas/schema.pl').

% TODO: bring episode and suggestion clauses under in their respective modules too.
:- consult(schemas:'threads/threads.pl').
:- consult('suggestions/suggestion.pl').
