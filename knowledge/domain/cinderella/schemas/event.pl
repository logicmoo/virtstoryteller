% action database
% @author kruizingaEE
% @date 2 may 2007

% operator((Head,
%   event,
%	PosPreconditions, NegPreconditions,
%	PosEffects,	NegEffects)) :-
% head(Head), preconditions(PosPreconditions), preconditions(NegPreconditions), effects(PosEffects, effects(NegEffects)
%
% head((actionName, [AgentID, Agens, Patiens, Target, Instrument | Vars])) :- .

% -- LoseClothing --
% One can lose clothing when one wears it, then it is supported by the target location.
schema([
    class(event),
    head([
        type('http://www.owl-ontologies.com/FabulaKnowledge.owl#LoseItem'),
        agens(Agens),
        patiens(Patiens),
        target(Target)
    ]),
    credibility(1),
    posPreconditions([
    	(Agens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#wears',			Patiens),
    	(Patiens, 	rdf:type, 				'http://www.owl-ontologies.com/StoryWorldCore.owl#Clothing'),
	    (Target,	rdf:type, 				'http://www.owl-ontologies.com/StoryWorldCore.owl#GeographicArea'),
	    (Agens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy', 	Target)
	    ]),
    posEffects([
	    (Patiens,	'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',		Target)    
    ]),
    negEffects([
    	(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy',		Agens)
    ])
]).

% -- StepOn --
% One can step on anything, for debug purposes it doesn't need to be there.
schema([
    class(event),
    head([
    	agens(Agens),
    	patiens(Patiens),
        type('http://www.owl-ontologies.com/FabulaKnowledge.owl#AccidentallyStepOn')
    ]),
    credibility(1)
]).
