% Only executable if load.pl was loaded
:- ensure_loaded('../../prolog/load.pl').

% Define Namespaces for domain
%:- rdf_db:rdf_register_ns(swc, 'http://www.owl-ontologies.com/StoryWorldCore.owl#').
:- rdf_db:rdf_load('ontology/StoryWorldCore.owl').   % ,[db(maingraph)] also has the disadvantage that each loaded source must have a DIFFERENT db, otherwise they are not loaded!
:- rdf_db:rdf_register_ns(ps, 'http://www.owl-ontologies.com/StoryWorldSettings/Pirates#').
:- rdf_db:rdf_load('ontology/PiratesSetting.owl').
:- rdf_db:rdf_load('setting/billyBones.ttl').
:- rdf_db:rdf_load('setting/island.ttl').
:- rdf_db:rdf_load('setting/anneBonney.ttl').
:- rdf_db:rdf_load('setting/ship.ttl').

% Load schemas into schema management module
:- consult(schemas:'schemas/schemas.pl').

:- consult(narrator:'narration/narration.pl').

% TODO: bring episode and suggestion clauses under in their respective modules too.
:- consult('suggestions/suggestion.pl').
