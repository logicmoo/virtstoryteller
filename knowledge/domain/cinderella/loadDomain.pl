% Only executable if load.pl was loaded
:- ensure_loaded('../../prolog/load.pl').

:- rdf_db:rdf_register_ns(swc, 'http://www.owl-ontologies.com/StoryWorldCore.owl#').
:- rdf_db:rdf_load('ontology/StoryWorldCore.owl').   % ,[db(maingraph)] also has the disadvantage that each loaded source must have a DIFFERENT db, otherwise they are not loaded!

% Define Namespaces StoryWorld
:- rdf_db:rdf_register_ns(cind, 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#').

% Load StoryWorld
:- rdf_db:rdf_load('ontology/cinderellaSetting.owl').

:- consult('schemas/schema.pl').