% TakeFrom
% TakeOut
% PutOn
% Dress
% Undress

% Steal stuff

action_schema([
	type(fabula:'StealFrom'),
	arguments([	agens(Agens), patiens(Patiens), target(Target), location(Loc) ]),
	duration(2),
	preconditions([
		% Agens is a pirate (only pirates can steal)
		condition(true, [
		    rule(Agens,	owlr:typeOrSubType,	ps:'Pirate'),
	    	fact(Patiens, swc:heldBy, Target)		    
		]),
		% Can't steal from yourself; this might speed up the planner
		condition(true, [
			rule(Agens, owlr:isNot, Target)
		]),
		% Both at same location
		condition(true, [
		    fact(Target,	    swc:supportedBy,	Loc),
		    fact(Agens,	swc:supportedBy,	Loc)
			]),
		% Can't take yourself
		condition(false, [
		    rule(Agens,	owlr:is,    Patiens),
		    rule(Agens, owlr:is,	Target)		  
		])
	]),
	effects([
		% Agent now has the thing
		condition(true, [
		    fact(Patiens,	swc:heldBy,		    Agens)		   
		]),
		% Thing is no longer where it was
		condition(false, [
		    fact(Patiens,	swc:heldBy,    Target)
		])
	])
]).




% ---- Agens takes Patiens from Target ----

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
]).
/*
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
*/