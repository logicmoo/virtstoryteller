% WalkFromTo
% WalkToFrom
% WalkFromToDoor
% WalkToFromDoor

% -------------------------------------------
% WalkFromTo -- walking on a road, "upstream"
% -------------------------------------------

% Agens walks from Location to Target  via Instrument
action_schema([
	type(fabula:'WalkFromTo'),
	arguments([	agens(Agens), target(Target), location(CurLoc), instrument(Road)]),
	duration(1),
	preconditions([
	    % Agens can do the action, either with his own body or using an instrument that can do the action
		condition(true, [
			fact(Agens,    swcr:knowsAction,       fabula:'WalkFromTo')
		]),
		
		% There is an "upstream" road to walk on, from where the instrument is
		condition(true, [
			fact(Agens,	swc:supportedBy,        CurLoc),
			fact(Road,	swc:fromGeographicArea, CurLoc),
			rule(Road,	owlr:typeOrSubType,     swc:'GroundWay'), 
			fact(Road,	swc:toGeographicArea,   Target)
		]),
		
	    % There is no door on this path
		condition(false, [
		    fact(Road,    swc:hasDoor,    _)   
		])
	]),
	effects([
	    % We are now at the target location
		condition(true, [
			fact(Agens,	swc:supportedBy,		Target)
		]),

	    % We are no longer at our current location
	    condition(false, [
			fact(Agens,	swc:supportedBy,		CurLoc)
		])
	])
]).


% --------------------------------------------------------------------
% WalkFromToDoor -- walking on a road that contains a door, "upstream"
% --------------------------------------------------------------------

% Agens walks from Location to Target through Door via Instrument
action_schema([
	type(fabula:'WalkFromToDoor'),
	arguments([ agens(Agens), target(Target), location(CurLoc), instrument(Road), door(Door) ]),
	duration(1),
	preconditions([
	    % Agens can do the action, either with his own body or using an instrument that can do the action		
		condition(true, [
			rule(Agens,	swcr:knowsAction, fabula:'WalkFromToDoor')
		]),
		
		% There is an "upstream" road to walk on, from where the instrument is		
		condition(true, [
			rule(Road, 	owlr:typeOrSubType, swc:'GroundWay'),
			fact(Road,	swc:toGeographicArea, Target),
			fact(Road,	swc:fromGeographicArea, CurLoc),
			fact(Agens,	swc:supportedBy,			CurLoc)
		]),
		
		% There is a door on this road, which is open
		condition(true, [
			fact(Road,    swc:hasDoor,    Door),
			fact(Door,    swc:hasOpenCloseProperty, swc:open)
		])
	]),
	effects([
	    % We are now at the target location	
		condition(true, [
			fact(Agens,	swc:supportedBy,		Target)
		]), 
		
	    % We are no longer at our current location		
	    condition(false, [
			fact(Agens,	swc:supportedBy,		CurLoc)			
		])
	])
]).
