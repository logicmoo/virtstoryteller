% TakeFrom
% TakeOut
% PutOn

schema([
	head([
		type('http://www.owl-ontologies.com/FabulaKnowledge.owl#TakeFrom'),
		agens(Agens), patiens(Patiens), instrument(Instrument), location(Location)
	]),
	class(action),
	duration(2),
	posPreconditions([
		    (Instrument,	swc:controlledBy,	Agens),
		    (Instrument,	swcr:knowsAction,	fabula:'TakeFrom'),
		    (Patiens,	    swc:supportedBy,	Location),
		    (Instrument,	swc:supportedBy,	Location),
		    (Instrument,	owlr:isNot,			Patiens)
		]),
	negPreconditions([
		    %(Instrument,	owlr:is,    Patiens)
		]),
	posEffects([
		    (Patiens,	swc:heldBy,		    Instrument)
		]),
	negEffects([
		    (Patiens,	swc:supportedBy,    Location)
		])
]).

schema([
	head([
		type('http://www.owl-ontologies.com/FabulaKnowledge.owl#TakeOut'),
		agens(Agens), patiens(Patiens), target(Target), instrument(Instrument), location(Location)
	]),
	class(action),
	duration(2),
	posPreconditions([
		    (Instrument,	swc:controlledBy,	Agens),
		    (Instrument,	swcr:knowsAction,	fabula:'TakeOut'),
		    (Patiens,	    swc:containedBy,	Target),
		    (Instrument, swc:supportedBy, Location),
		    (Target,	swc:supportedBy,	Location)
		]),
	negPreconditions([
		    (Instrument,	owlr:is,    Patiens)
		]),
	posEffects([
		    (Patiens,	swc:heldBy,		    Instrument)
		]),
	negEffects([
		    (Patiens,	swc:containedBy,    Target)
		])
]).

schema([
	head([
		type('http://www.owl-ontologies.com/FabulaKnowledge.owl#PutOn'), 
		agens(Agens), patiens(Patiens), instrument(Instrument), target(Target)
	]),
	class(action),
	duration(3),
	posPreconditions([
		(Instrument,	swc:controlledBy,	Agens),
		(Instrument,	swcr:knowsAction, fabula:'PutOn'),
		(Patiens,	swc:heldBy,			Instrument),
		(Instrument,	swc:supportedBy,		Target)
		]),
	negPreconditions([
		]),
	posEffects([
		(Patiens,	swc:supportedBy,		Target)
		]),
	negEffects([
		(Patiens,	swc:heldBy,			Instrument)
		])
]).