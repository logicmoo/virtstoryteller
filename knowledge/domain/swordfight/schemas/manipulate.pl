% OpenDoor
% OpenContainer

% -------------------------------------------------
% OpenDoor -- opening something that can be opened
% -------------------------------------------------
action_schema([
	type(fabula:'OpenDoor'),
    arguments([    agens(Agens), patiens(Patiens), road(Road) ]),
    duration(1),
    preconditions([
    	
    	 % Agens can do the action
		condition(true, [
			rule(Agens,	swcr:knowsAction, fabula:'OpenDoor')
		]),
		
		% Instrument is located "at" a door that is openable, but closed.
		condition(true, [
			fact(Agens, swc:supportedBy, CurrLoc),
			fact(Road, swc:connectedToGeographicArea, CurrLoc), % CAREFUL, this disables any improvisation actions with effect "fromGeographicArea" or "toGeographicArea".
			fact(Road, swc:hasDoor, Patiens),
			fact(Patiens, swc:hasOpenCloseProperty, swc:closed),
			fact(Patiens, swc:hasOpenCloseProperty, swc:openable)
		]),
		
	    % The door must not be open
        condition(false, [
	        fact(Patiens, swc:hasOpenCloseProperty, swc:open)
	    ])
		
	]),
	effects([
	    % The door is now open
		condition(true, [
		    fact(Patiens, swc:hasOpenCloseProperty, swc:open)
		]),

		% The door is no longer closed
		condition(false, [
			fact(Patiens, swc:hasOpenCloseProperty, swc:closed)
		])
	])
]).

% ------------------------------------
% OpenContainer -- opening a container
% ------------------------------------
action_schema([
	type(fabula:'OpenContainer'),
    arguments([ agens(Agens), patiens(Patiens) ]),
    duration(1),
    preconditions([
		% Agens can do the action
    	condition(true, [
			rule(Agens,	swcr:knowsAction, fabula:'OpenContainer')
		]),
		
		% Instrument is located "at" a door that is openable, but closed.
		condition(true, [
			fact(Agens, swc:supportedBy, CurrLoc),
			fact(Patiens, swc:supportedBy, CurrLoc),
			rule(Patiens, owlr:typeOrSubType, swc:'Container'),
			fact(Patiens, swc:hasOpenCloseProperty, swc:closed),
			fact(Patiens, swc:hasOpenCloseProperty, swc:openable)
		]),
		
		% The container must not be open
		condition(false, [
	        fact(Patiens, swc:hasOpenCloseProperty, swc:open)
	    ])
	]),
	
	effects([
	    % The container is now open
		condition(true, [
		    fact(Patiens, swc:hasOpenCloseProperty, swc:open)
		]),
	    % The container is no longer closed
	    condition(false, [
	    	fact(Patiens, swc:hasOpenCloseProperty, swc:closed)
	    ])
	])
]).