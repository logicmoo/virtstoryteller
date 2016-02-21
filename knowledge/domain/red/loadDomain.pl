% Only executable if load.pl was loaded
:- ensure_loaded('../../prolog/load.pl').

% Define Namespaces for domain
:- rdf_db:rdf_register_ns(red, 'http://www.owl-ontologies.com/Red.owl#').
:- rdf_db:rdf_load('ontology/Red.owl').
:- rdf_db:rdf_load('setting/setting.ttl').
%:- rdf_db:rdf_load('setting/setting_from_grandma_plot.ttl').

% Load schemas into schema management module
:- consult(schemas:'schemas/schema.pl').

:- consult(narrator:'narration/narration.pl').

% TODO: bring episode and suggestion clauses under in their respective modules too.
:- consult(schemas:'threads/threads.pl').
:- consult('suggestions/suggestion.pl').
