% TakeFrom
% TakeOut
% PutOn
% Dress
% Undress

schema([
	head([
		type('http://www.owl-ontologies.com/FabulaKnowledge.owl#TakeFrom'),
		agens(Agens), patiens(Patiens), location(Location)
	]),
	class(action),
	duration(2),
	posPreconditions([
		    (Agens,	swcr:knowsAction,	fabula:'TakeFrom'),
		    (Patiens,	    swc:supportedBy,	Location),
		    (Agens,	swc:supportedBy,	Location)
		]),
	negPreconditions([
		    (Agens,	owlr:is,    Patiens)
		]),
	posEffects([
		    (Patiens,	swc:heldBy,		    Agens)
		]),
	negEffects([
		    (Patiens,	swc:supportedBy,    Location)
		])
]).

schema([
	head([
		type('http://www.owl-ontologies.com/FabulaKnowledge.owl#TakeOut'),
		agens(Agens), patiens(Patiens), target(Target), location(Location)
	]),
	class(action),
	duration(2),
	posPreconditions([
		    (Agens,	swcr:knowsAction,	fabula:'TakeOut'),
		    (Patiens,	    swc:containedBy,	Target),
		    (Agens, swc:supportedBy, Location),
		    (Target,	swc:supportedBy,	Location)
		]),
	negPreconditions([
		    (Agens,	owlr:is,    Patiens)
		]),
	posEffects([
		    (Patiens,	swc:heldBy,		    Agens)
		]),
	negEffects([
		    (Patiens,	swc:containedBy,    Target)
		])
]).

schema([
	head([
		type('http://www.owl-ontologies.com/FabulaKnowledge.owl#PutOn'), 
		agens(Agens), patiens(Patiens), target(Target)
	]),
	class(action),
	duration(3),
	posPreconditions([
		(Agens,	swcr:knowsAction, fabula:'PutOn'),
		(Patiens,	swc:heldBy,			Agens),
		(Agens,	swc:supportedBy,		Target)
		]),
	negPreconditions([
		]),
	posEffects([
		(Patiens,	swc:supportedBy,		Target)
		]),
	negEffects([
		(Patiens,	swc:heldBy,			Agens)
		])
]).

/*
	Swartjes: PutOnSelf is identical to PutOn with the target bound to Agens. The planner will find it automatically.
	
	schema([
	head(
		[type('http://www.owl-ontologies.com/FabulaKnowledge.owl#PutOnSelf'), 
		agens(Agens), patiens(Patiens), instrument(Instrument), target(Agens)
	]),
	class(action),
	duration(3),
	posPreconditions([
		fact(Instrument,	swc:controlledBy,	Agens),
		(Instrument,	swcr:knowsAction,			fabula:'PutOn'),
		(Patiens,	swc:heldBy,			Instrument),
		(Instrument,	swc:supportedBy,		Location)
		]),
	negPreconditions([
		]),
	posEffects(	[
		(Patiens,	swc:supportedBy,		Patiens)
		]),
	negEffects([
		(Patiens,	swc:heldBy,			Instrument)
		])
]).*/

schema([
	head(
		[type('http://www.owl-ontologies.com/FabulaKnowledge.owl#Dress'), 
		agens(Agens), patiens(Patiens)
	]),
	class(action),
	duration(15),
	posPreconditions([
		(Agems,	swcr:knowsAction,		fabula:'Dress'),
		(Patiens,	swc:heldBy,			Agens),
		(Patiens, 	owlr:typeOrSubType, swc:'WearableItem')
		%(Patiens, 	rdf:type, 			swc:'Clothing')
		]),
	negPreconditions([
		]),
	posEffects([
		(Patiens,	swc:wornBy,			Agens)
		]),
	negEffects([
		(Patiens,	swc:heldBy,			Agens)
		])
]).

schema([
	head(
		[type('http://www.owl-ontologies.com/FabulaKnowledge.owl#Undress'), 
		agens(Agens), patiens(Patiens)
	]),
	class(action),
	duration(17),
	posPreconditions([
		(Agens,	swcr:knowsAction,	fabula:'Undress'),
		(Patiens,	    swc:wornBy,			Agens)
		]),
	negPreconditions([
		]),
	posEffects([
		(Patiens,	swc:heldBy,			Agens)
		]),
	negEffects([
		(Patiens,	swc:wornBy,			Agens)
		])
]).
