% improvisation database
% @author kruizingaEE
% @date 13-06-2007

% operator((improvisation,Head, Duration,
%	PosPreconditions, NegPreconditions,
%	PosEffects,	NegEffects)) :-
% head(Head),
%% duration(Duration),
% preconditions(PosPreconditions), preconditions(NegPreconditions),
% effects(PosEffects), effects(NegEffects)
%
% head((actionName, [Agens, Patiens, Target, Instrument | Vars])) :- ?.

/*
operator((
	('http://www.owl-ontologies.com/FabulaKnowledge.owl#CreateClothing', [_Agens, Patiens, none, Instrument, CurrLoc]),
	improvisation,
	1,
[
],
[
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			_Somewhere)
],
[
(Patiens,			rdf:type,									'http://www.owl-ontologies.com/StoryWorldCore.owl#Clothing'),
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			CurrLoc)
],
[
])
).
*/

/*operator((
	('http://www.owl-ontologies.com/FabulaKnowledge.owl#CreateHelper', [Agens, Patiens, none, Instrument, CurrLoc]),
	improvisation,
	1,
[
(Instrument,				'http://www.owl-ontologies.com/StoryWorldCore.owl#controlledBy',		Agens),
(Instrument,				'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			CurrLoc),
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#improvroom'),
(Patiens,			swcr:type,																'http://www.owl-ontologies.com/StoryWorldCore.owl#Organism')
],
[
],
[
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#controlledBy',		Agens),
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			CurrLoc)
],
[
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#improvroom')
])
).*/

/*operator((
	('http://www.owl-ontologies.com/FabulaKnowledge.owl#GetFromMagicStore', [Agens, Patiens, none, Instrument, CurrLoc]),
	improvisation,
	1,
[
(Instrument,				'http://www.owl-ontologies.com/StoryWorldCore.owl#controlledBy',		Agens),
(Instrument,				'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			CurrLoc),
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#improvroom')
%(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			Target)
%(Patiens,			swcr:type,																'http://www.owl-ontologies.com/StoryWorldCore.owl#Organism')
],
[
],
[
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			CurrLoc)
],
[
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#improvroom')
%(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			Target)
])
).*/

/*
operator((
	('http://www.owl-ontologies.com/FabulaKnowledge.owl#MagicTakeControl', [_Agens, Patiens, none, Instrument, none]),
	improvisation,
	1,
[
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#improvroom'),
(Patiens,			swcr:type,																'http://www.owl-ontologies.com/StoryWorldCore.owl#Organism')
],
[
],
[
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#controlledBy',			Agens)
],
[
])
).
*/


schema([    
    head([
        type('http://www.owl-ontologies.com/FabulaKnowledge.owl#IntroduceArm'),
        agens(Ag)
        ]),
    class(improvisation), 
    posPreconditions([
		(Ag, swcr:type, 'http://www.owl-ontologies.com/StoryWorldCore.owl#Human')    	
    ]),
    posEffects([
		(Ag, 'http://www.owl-ontologies.com/StoryWorldCore.owl#hasBodyPart', 'http://www.owl-ontologies.com/StoryWorldCore.owl#arm'),
		('http://www.owl-ontologies.com/StoryWorldCore.owl#arm', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.owl-ontologies.com/StoryWorldCore.owl#Arm')
    ])]).

schema([    
    head([
        type('http://www.owl-ontologies.com/FabulaKnowledge.owl#IntroduceBrace'),
        agens(Ag),patiens(Pa)
        ]),
    class(improvisation), 
    posPreconditions([
		(Ag, 'http://www.owl-ontologies.com/StoryWorldCore.owl#hasBodyPart', Pa),
		(Pa, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.owl-ontologies.com/StoryWorldCore.owl#Arm')    	
    ]),
    
    posEffects([
    
        ('http://www.owl-ontologies.com/FabulaKnowledge.owl#brace', 'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy', Ag)
    ])]).
    
schema([    
    head([
        type('http://www.owl-ontologies.com/FabulaKnowledge.owl#CreateNecklace'),
        agens(Ag)
        ]),
    class(improvisation), 
    posPreconditions([
    ]),
    
    posEffects([
    
        ('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#necklace', 'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy', Ag)
    ])]).    