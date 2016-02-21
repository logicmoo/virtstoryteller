% WalkFromTo
% WalkToFrom
% WalkFromToDoor
% WalkToFromDoor

% -------------------------------------------
% WalkFromTo -- walking on a road, "upstream"
% -------------------------------------------
schema([
	head([
		type('http://www.owl-ontologies.com/FabulaKnowledge.owl#WalkFromTo'),
		agens(Agens), target(Target), instrument(Instrument), location(CurLoc), road(Road), door(Door), length(Len)
		]),
	class(action),
	duration(1),
	posPreconditions([
	    % Agens can do the action, either with his own body or using an instrument that can do the action
		(Instrument,    swc:controlledBy,       Agens),
		(Instrument,    swcr:knowsAction,       fabula:'WalkFromTo'),
		
		% There is an "upstream" road to walk on, from where the instrument is
		(Instrument,	swc:supportedBy,        CurLoc),
		(Road,	owlr:typeOrSubType,     swc:'GroundWay'), 
		(Road,	swc:fromGeographicArea, CurLoc),
		(Road,	swc:toGeographicArea,   Target),
		(Road,	swcr:length,			Len)
		]),
	negPreconditions([
	    % There is no door on this path
	    (Road,    swc:hasDoor,    Door)
		]),
	posEffects([
	    % We are now at the target location
		(Instrument,	swc:supportedBy,		Target)
		]),
	negEffects([
	    % We are no longer at our current location
		(Instrument,	swc:supportedBy,		CurLoc)
		])
]).

% ---------------------------------------------
% WalkToFrom -- walking on a road, "downstream"
% ---------------------------------------------
/*schema([
	head([
		type('http://www.owl-ontologies.com/FabulaKnowledge.owl#WalkToFrom'),
		agens(Agens), target(Target), instrument(Instrument), location(CurLoc), road(Road), door(Door), length(Len)
		]),
	class(action),
	duration(1),
	posPreconditions([
	    % Agens can do the action, either with his own body or using an instrument that can do the action	
		(Instrument,	swc:controlledBy,		Agens),
		(Instrument,	swcr:knowsAction,		fabula:'WalkToFrom'),
		
		% There is a "downstream" road to walk on, from where the instrument is		
		(Instrument,	swc:supportedBy,			CurLoc),
		(Road,	swc:toGeographicArea,	CurLoc),
		(Road, 	owlr:typeOrSubType, swc:'GroundWay'),
		(Road,	swc:fromGeographicArea,	Target),
		(Road,	swcr:length, Len)
		]),
	negPreconditions([
	    % There is no door on this path
	    (Road,    swc:hasDoor,    Door)	
		]),
	posEffects([
	    % We are now at the target location	
		(Instrument,	swc:supportedBy,		Target)
		]),
	negEffects([
	    % We are no longer at our current location	
		(Instrument,	swc:supportedBy,		CurLoc)
		])
]).*/

% --------------------------------------------------------------------
% WalkFromToDoor -- walking on a road that contains a door, "upstream"
% --------------------------------------------------------------------
schema([
	head([
		type('http://www.owl-ontologies.com/FabulaKnowledge.owl#WalkFromToDoor'),
		agens(Agens), target(Target), instrument(Instrument), location(CurLoc), road(Road), door(Door), length(Len)
		]),
	class(action),
	duration(1),
	posPreconditions([
	    % Agens can do the action, either with his own body or using an instrument that can do the action		
		(Instrument,	swc:controlledBy, Agens),
		(Instrument,	swcr:knowsAction, fabula:'WalkFromTo'),
		
		% There is an "upstream" road to walk on, from where the instrument is		
		(Road, 	owlr:typeOrSubType, swc:'GroundWay'),
		(Road,	swc:toGeographicArea, Target),
		(Road,	swc:fromGeographicArea, CurLoc),
		(Road,	swcr:length,				Len),
		(Instrument,	swc:supportedBy,			CurLoc),
		
		% There is a door on this road, which is open
		(Road,    swc:hasDoor,    Door),
		(Door,    swc:hasOpenCloseProperty, swc:open)
		]),
	negPreconditions([]),
	posEffects([
	    % We are now at the target location		
		(Instrument,	swc:supportedBy,		Target)
		]),
	negEffects([
	    % We are no longer at our current location		
		(Instrument,	swc:supportedBy,		CurLoc)
		])
]).

% ----------------------------------------------------------------------
% WalkToFromDoor -- walking on a road that contains a door, "downstream"
% ----------------------------------------------------------------------
/*schema([
	head([
		type('http://www.owl-ontologies.com/FabulaKnowledge.owl#WalkToFromDoor'),
		agens(Agens), target(Target), instrument(Instrument), location(CurLoc), road(Road), door(Door), length(Len)
		]),
	class(action),
	duration(1),
	posPreconditions([
	    % Agens can do the action, either with his own body or using an instrument that can do the action			
		(Instrument,	swc:controlledBy,		Agens),
		(Instrument,	swcr:knowsAction, fabula:'WalkToFrom'),
		
		% There is a "downstream" road to walk on, from where the instrument is		
		(Road, 	owlr:typeOrSubType, swc:'GroundWay'),
		(Road,	swc:fromGeographicArea,	Target),
		(Road,	swc:toGeographicArea,	CurLoc),
		(Road,	swcr:length,				Len),
		(Instrument,	swc:supportedBy,			CurLoc),
		
		% There is a door on this road, which is open
		(Road,    swc:hasDoor,    Door),
		(Door,    swc:hasOpenCloseProperty, swc:open)
		]),
	negPreconditions([	]),
	posEffects([
	    % We are now at the target location		
		(Instrument,	swc:supportedBy, Target)
		]),
	negEffects([
	    % We are no longer at our current location		
		(Instrument,	swc:supportedBy, CurLoc)
		])
]).*/