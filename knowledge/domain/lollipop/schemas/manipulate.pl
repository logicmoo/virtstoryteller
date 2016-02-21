% OpenDoor
% OpenContainer

% -------------------------------------------------
% OpenDoor -- opening something that can be opened
% -------------------------------------------------
schema([
    head([
        type('http://www.owl-ontologies.com/FabulaKnowledge.owl#OpenDoor'),
        agens(Agens), patiens(Patiens), instrument(Instrument), road(Road)
        ]),
    class(action),
    duration(1),
    posPreconditions([
    	 % Agens can do the action, either with his own body or using an instrument that can do the action	
		(Instrument,	swc:controlledBy,	Agens),
		(Instrument,	swcr:knowsAction, fabula:'OpenDoor'),
		
		% Instrument is located "at" a door that is openable, but closed.
		(Instrument, swc:supportedBy, CurrLoc),
		(Road, swc:connectedToGeographicArea, CurrLoc), % CAREFUL, this disables any improvisation actions with effect "fromGeographicArea" or "toGeographicArea".
		(Road, swc:hasDoor, Patiens),
		(Patiens, swc:hasOpenCloseProperty, swc:openable),
		(Patiens, swc:hasOpenCloseProperty, swc:closed)
	]),
	negPreconditions([
	    % The door must not be open
        (Patiens, swc:hasOpenCloseProperty, swc:open)
	]),
	posEffects([
	    % The door is now open
	    (Patiens, swc:hasOpenCloseProperty, swc:open)	    
	]),
	negEffects([
	    % The door is no longer closed
	    (Patiens, swc:hasOpenCloseProperty, swc:closed)
	])
]).

% ------------------------------------
% OpenContainer -- opening a container
% ------------------------------------
schema([
    head([
        type('http://www.owl-ontologies.com/FabulaKnowledge.owl#OpenContainer'),
        agens(Agens), patiens(Patiens), instrument(Instrument)
        ]),
    class(action),
    duration(1),
    posPreconditions([
    	 % Agens can do the action, either with his own body or using an instrument that can do the action	
		(Instrument,	swc:controlledBy,	Agens),
		(Instrument,	swcr:knowsAction, fabula:'OpenContainer'),
		
		% Instrument is located "at" a door that is openable, but closed.
		(Instrument, swc:supportedBy, CurrLoc),
		(Patiens, swc:supportedBy, CurrLoc),
		(Patiens, owlr:typeOrSubType, swc:'Container'),
		(Patiens, swc:hasOpenCloseProperty, swc:openable),
		(Patiens, swc:hasOpenCloseProperty, swc:closed)
	]),
	negPreconditions([
	    % The container must not be open
        (Patiens, swc:hasOpenCloseProperty, swc:open)
	]),
	posEffects([
	    % The container is now open
	    (Patiens, swc:hasOpenCloseProperty, swc:open)	    
	]),
	negEffects([
	    % The container is no longer closed
	    (Patiens, swc:hasOpenCloseProperty, swc:closed)
	])
]).