% TakeFrom
% TakeOut
% PutOn
% Dress
% Undress

% ---- Agens takes Patiens from Target ----
/*
action_schema([
	type(fabula:'TakeFrom'),
	arguments([	agens(Agens), patiens(Patiens), target(Target) ]),
	duration(2),
	preconditions([
		% Agens can do the action
		condition(true, [
		    rule(Agens,	swcr:knowsAction,	fabula:'TakeFrom')
		]),
		% Both at same location
		condition(true, [
		    fact(Patiens,	    swc:supportedBy,	Target),
		    fact(Agens,	swc:supportedBy,	Target)		
		]),
		% Can't take yourself
		condition(false, [
		    rule(Agens,	owlr:is,    Patiens)		  
		])
	]),
	effects([
		% Agent now has the thing
		condition(true, [
		    fact(Patiens,	swc:heldBy,		    Agens)		   
		]),
		% Thing is no longer where it was
		condition(false, [
		    fact(Patiens,	swc:supportedBy,    Target)
		])
	])
]).*/

% ---- Agens takes Patiens out of Target at Location ----
action_schema([
	type(fabula:'TakeOut'),
	arguments([	agens(Agens), patiens(Patiens), target(Target), location(Location) ]),
	duration(2),
	preconditions([
		% Agent can do the action
		condition(true, [
		    rule(Agens,	swcr:knowsAction,	fabula:'TakeOut')
		]),
		% Thing to take out is at same location as agent
		condition(true, [
		    fact(Patiens,	    swc:containedBy,	Target),
		    fact(Target,	swc:supportedBy,	Location),
		    fact(Agens, swc:supportedBy, Location)  
		]),
		% Can't take yourself out
		condition(false, [
		    rule(Agens,	owlr:is,    Patiens)		 
		])		
	]),
	effects([
		% Now has thing
		condition(true, [
		    fact(Patiens,	swc:heldBy,		    Agens)	   
		]),
		% Thing no longer in container
		condition(false, [
		    fact(Patiens,	swc:containedBy,    Target)
		])
	])
]).

% Agens puts Patiens on Target using Instrument
/*
action_schema([
	type(fabula:'PutOn'), 
	arguments([	agens(Agens), patiens(Patiens), target(Target)]),
	duration(3),
	preconditions([
		% Agent can do the action
		condition(true, [
			rule(Agens,	swcr:knowsAction, fabula:'PutOn')
		]),
		% Agent has the object and is standing somewhere
		condition(true, [
			fact(Patiens,	swc:heldBy,			Agens),
			fact(Agens,	swc:supportedBy,		Target)
		])
	]),
	effects([
		% Object is now on the floor
		condition(true, [
			fact(Patiens,	swc:supportedBy,		Target)
		]),
		% Agent doesnt have the object anymore
		condition(false, [
			fact(Patiens,	swc:heldBy,			Agens)
		])
	])
]).*/

% Agens dresses with Patiens using Instrument
/*
action_schema([
	type(fabula:'Dress'), 
	arguments([	agens(Agens), patiens(Patiens) ]),
	duration(15),
	preconditions([
		% Agent can do the action
		condition(true, [
			rule(Agens,	swcr:knowsAction,		fabula:'Dress')
		]),
		% Clothing is held by the agent
		condition(true, [
			rule(Patiens, 	owlr:typeOrSubType, swc:'WearableItem'),
			fact(Patiens,	swc:heldBy,			Agens)			
		])
	]),
	effects([
		% Agent wears the clothing
		condition(true, [
			fact(Patiens,	swc:wornBy,			Agens)		
		]),
		% Agent doesnt hold the clothing anymore
		condition(false, [
			fact(Patiens,	swc:heldBy,			Agens)
		])
	])
]).

% Agens takes off Patiens using Instrument
action_schema([
	type(fabula:'Undress'), 
	arguments([	agens(Agens), patiens(Patiens)	]),
	duration(17),
	preconditions([
		% Agent can do the action
		condition(true, [
			rule(Agens,	swcr:knowsAction,	fabula:'Undress')
		]),
		% Clothing is worn by agent
		condition(true, [
			fact(Patiens,	    swc:wornBy,			Agens)
		])
	]),
	effects([
		% Agent now holds the clothing again
		condition(true, [
			fact(Patiens,	swc:heldBy,			Agens)
		]),
		% Agent no longer wears the clothing
		condition(false, [
			fact(Patiens,	swc:wornBy,			Agens)
		])
	])
]).*/
