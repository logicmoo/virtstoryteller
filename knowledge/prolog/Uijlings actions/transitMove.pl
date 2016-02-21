%%%
% New version by Edze Kruizinga
% @date 9 mrt 2007
% added ( ) around action to make the action name available as a variable value
% changed simple action names into OWL ontology names

% File containing all TransitMoves

% transitMove
% 	groundMove
% 		ambulate
% 			walk
% 				walkFromToDoor
% 				walkToFromDoor
% 				walkFromTo
% 				walkToFrom


%%%%
% transitMove
%%%%
action
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#TransitMove', (AgentID, Agens, Patiens, Target, Instrument, [Vars, CurrLoc]))
superAction
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Action', (AgentID, Agens, Patiens, Target, Instrument, Vars))
primitiveAction
		false
preconditions
		% The agens must be supportedBy a lowestLevel GeographicArea
		(Agens, swc:supportedBy, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		% The agens must not be attached
		unp (Agens, swc:attachedBy, _SomeAttachingObject)
interEffects
	add
		(Agens, swc:isOnTransitWay, Instrument)
	del
		(Agens, swc:supportedBy, CurrLoc)
effects
	add
		(Agens, swc:supportedBy, Target)
	del
		(Agens, swc:isOnTransitWay, Instrument)
. 
%%%
% End of transitMove
%%%

%%%%
% groundMove
% %%

action
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#GroundMove', (AgentID, Agens, Patiens, Target, Instrument, Vars))
superAction
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#TransitMove', (AgentID, Agens, Patiens, Target, Instrument, Vars))
primitiveAction
		false
preconditions
		% One must move over ground
		(Instrument, rdf:type, swc:'GroundWay')
	% nextBlock
		% The path must be wide (width) enough
		% 26 jan 2007, disabled because we will not use numbers, Kruizinga
		% (Instrument, swc:width, InstrumentWidth) and
		% (Agens, swc:width, AgensWidth) and
		% math(AgensWidth < InstrumentWidth)
. 

%%%
% End of GroundMove
% %%

%%%
%Ambulate
% 

action
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Ambulate', (AgentID, Agens, Patiens, Target, Instrument, Vars))
superAction
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#GroundMove', (AgentID, Agens, Patiens, Target, Instrument, Vars))
primitiveAction
		false
.

%%%%
% End of ambulate
%%%%

%%%%
% walk
% %%
action
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Walk', (AgentID, Agens, Patiens, Target, Instrument, Vars))
superAction
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Ambulate', (AgentID, Agens, Patiens, Target, Instrument, Vars))
primitiveAction
		false
preconditions
		% The agent must be able to walk
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:'Walk', owlr:classOrSubClassOf, TheAgensAction)
.

%%%%
% End of Walk
% %%%

%%%%
% WalkFromToDoor
% %%
action
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#WalkFromToDoor', (AgentID, Agens, Patiens, Target, Instrument, [Vars, CurrLoc, TheDoor, DoorProp]))
superAction
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Walk', (AgentID, Agens, Patiens, Target, Instrument, Vars))
primitiveAction
		true
interDuration
		3
duration
		6
preconditions
		(Instrument, swc:fromGeographicArea, CurrLoc) and
		(Agens, swc:supportedBy, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		% The door must be open
		(Instrument, swc:hasDoor, TheDoor) and
		(TheDoor, swc:hasOpenCloseProperties, DoorProp) and
		(DoorProp, swc:isOpen, true) 
	nextBlock
		(Instrument, swc:toGeographicArea, Target)
. % End of walkFromToDoor


%%%%%
% walkToFromDoor
action
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#WalkToFromDoor', (AgentID, Agens, Patiens, Target, Instrument, [Vars, CurrLoc, TheDoor, DoorProp]))
superAction
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Walk', (AgentID, Agens, Patiens, Target, Instrument, Vars))
primitiveAction
		true
interDuration
		3
duration
		6
preconditions
		(Instrument, swc:hasDoor, TheDoor) and
		(TheDoor, swc:hasOpenCloseProperties, DoorProp) and
		(DoorProp, swc:isOpen, true)
	nextBlock
		(Instrument, swc:fromGeographicArea, Target)
	nextBlock
		(Agens, swc:supportedBy, CurrLoc) and
		(Instrument, swc:toGeographicArea, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
. % End of walkToFromDoor


%%%%%
% walkFromTo
action
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#WalkFromTo', (AgentID, Agens, Patiens, Target, Instrument, Vars))
superAction
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Walk', (AgentID, Agens, Patiens, Target, Instrument, Vars))
primitiveAction
		true
interDuration
		3
duration
		6
preconditions
		(Instrument, swc:fromGeographicArea, CurrLoc) and
		(Agens, swc:supportedBy, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		(Instrument, swc:toGeographicArea, Target)
	nextBlock
		unp (Instrument, swc:hasDoor, _Door)
. % End of walkFromTo

%%%%%
% walkToFrom
action
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#WalkToFrom', (AgentID, Agens, Patiens, Target, Instrument, Vars))
superAction
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Walk', (AgentID, Agens, Patiens, Target, Instrument, Vars))
primitiveAction
		true
interDuration
		3
duration
		6
preconditions
		(Instrument, swc:fromGeographicArea, Target)
	nextBlock
		(Agens, swc:supportedBy, CurrLoc) and
		(Instrument, swc:toGeographicArea, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		unp (Instrument, swc:hasDoor, _Door)
. % End of walkToFrom


/*
action
		flyFromTo(Agens, Patiens, Target, Instrument, [])
duration
		6
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Fly, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Instrument, rdf:type, swc:TransitWay)
	nextBlock
		(Instrument, swc:fromGeographicArea, CurrLoc) and
		(Agens, swc:supportedBy, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		not(Agens, swc:attachedBy, SomeObjectA)
	nextBlock
		(Agens, swc:width, AgensWidth) and
		(Instrument, swc:flyingWidth, FlyingWidth) and
		(> FlyingWidth AgensWidth)
	nextBlock
		(Instrument, swc:toGeographicArea, Target)
	nextBlock
		not(Instrument, swc:hasDoor, ADoor)
	nextBlock
		(Agens, swc:controlledBy, ?AGENTID)
interEffects
	add
		(Agens, swc:isOnTransitWay, Instrument) and
	del
		(Agens, swc:supportedBy, CurrLoc) and
effects
	add
		(Agens, swc:supportedBy, Target) and
	del
		(Agens, swc:isOnTransitWay, Instrument) and 
. % End of flyFromTo


action
		runToFrom(Agens, Patiens, Target, Instrument, [])
duration
		6
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Run, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Instrument, swc:width, PathWidth) and
		(Agens, swc:width, AgensWidth) and
		(> PathWidth AgensWidth)
	nextBlock
		(Instrument, swc:fromGeographicArea, Target)
	nextBlock
		not(Agens, swc:attachedBy, SomeObjectA)
	nextBlock
		(Agens, swc:supportedBy, CurrLoc) and
		(Instrument, swc:toGeographicArea, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		(Instrument, rdf:type, swc:GroundWay)
	nextBlock
		not(Instrument, swc:hasBlockingObstacle, SomeObstacle)
	nextBlock
		not(Instrument, swc:hasDoor, ADoor)
	nextBlock
		(Agens, swc:controlledBy, ?AGENTID)
interEffects
	add
		(Agens, swc:isOnTransitWay, Instrument) and
	del
		(Agens, swc:supportedBy, CurrLoc) and
effects
	add
		(Agens, swc:supportedBy, Target) and
	del
		(Agens, swc:isOnTransitWay, Instrument) and 
. % End of runToFrom


action
		runToFromDoor(Agens, Patiens, Target, Instrument, [])
duration
		6
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Run, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Instrument, swc:width, PathWidth) and
		(Agens, swc:width, AgensWidth) and
		(> PathWidth AgensWidth)
	nextBlock
		(Instrument, swc:fromGeographicArea, Target)
	nextBlock
		not(Agens, swc:attachedBy, SomeObjectA)
	nextBlock
		(Agens, swc:supportedBy, CurrLoc) and
		(Instrument, swc:toGeographicArea, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		(Instrument, rdf:type, swc:GroundWay)
	nextBlock
		not(Instrument, swc:hasBlockingObstacle, SomeObstacle)
	nextBlock
		(Instrument, swc:hasDoor, TheDoor) and
		(DoorProperty, swc:isOpen, true) and
		(TheDoor, swc:hasOpenCloseProperties, DoorProperty)
	nextBlock
		(Agens, swc:controlledBy, ?AGENTID)
interEffects
	add
		(Agens, swc:isOnTransitWay, Instrument) and
	del
		(Agens, swc:supportedBy, CurrLoc) and
effects
	add
		(Agens, swc:supportedBy, Target) and
	del
		(Agens, swc:isOnTransitWay, Instrument) and 
. % End of runToFromDoor




action
		cycleFromToDoor(Agens, Patiens, Target, Instrument, [])
duration
		6
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Cycle, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Instrument, swc:width, PathWidth) and
		(Agens, swc:width, AgensWidth) and
		(> PathWidth AgensWidth)
	nextBlock
		(Instrument, swc:fromGeographicArea, CurrLoc) and
		(Agens, swc:supportedBy, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		not(Agens, swc:attachedBy, SomeObjectA)
	nextBlock
		(Instrument, rdf:type, swc:GroundWay)
	nextBlock
		(Instrument, swc:toGeographicArea, Target)
	nextBlock
		not(Instrument, swc:hasBlockingObstacle, SomeObstacle)
	nextBlock
		(Instrument, swc:hasDoor, TheDoor) and
		(DoorProperty, swc:isOpen, true) and
		(TheDoor, swc:hasOpenCloseProperties, DoorProperty)
	nextBlock
		(Agens, swc:controlledBy, ?AGENTID)
interEffects
	add
		(Agens, swc:isOnTransitWay, Instrument) and
	del
		(Agens, swc:supportedBy, CurrLoc) and
effects
	add
		(Agens, swc:supportedBy, Target) and
	del
		(Agens, swc:isOnTransitWay, Instrument) and 
. % End of cycleFromToDoor


action
		flyToFrom(Agens, Patiens, Target, Instrument, [])
duration
		6
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Fly, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Instrument, rdf:type, swc:TransitWay)
	nextBlock
		(Instrument, swc:fromGeographicArea, Target)
	nextBlock
		not(Agens, swc:attachedBy, SomeObjectA)
	nextBlock
		(Agens, swc:supportedBy, CurrLoc) and
		(Instrument, swc:toGeographicArea, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		(Agens, swc:width, AgensWidth) and
		(Instrument, swc:flyingWidth, FlyingWidth) and
		(> FlyingWidth AgensWidth)
	nextBlock
		not(Instrument, swc:hasDoor, ADoor)
	nextBlock
		(Agens, swc:controlledBy, ?AGENTID)
interEffects
	add
		(Agens, swc:isOnTransitWay, Instrument) and
	del
		(Agens, swc:supportedBy, CurrLoc) and
effects
	add
		(Agens, swc:supportedBy, Target) and
	del
		(Agens, swc:isOnTransitWay, Instrument) and 
. % End of flyToFrom


action
		cycleFromTo(Agens, Patiens, Target, Instrument, [])
duration
		6
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Cycle, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Instrument, swc:width, PathWidth) and
		(Agens, swc:width, AgensWidth) and
		(> PathWidth AgensWidth)
	nextBlock
		(Instrument, swc:fromGeographicArea, Target)
	nextBlock
		not(Agens, swc:attachedBy, SomeObjectA)
	nextBlock
		(Agens, swc:supportedBy, CurrLoc) and
		(Instrument, swc:toGeographicArea, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		not(Instrument, swc:hasDoor, SomeDoor)
	nextBlock
		(Instrument, rdf:type, swc:GroundWay)
	nextBlock
		not(Instrument, swc:hasBlockingObstacle, SomeObstacle)
	nextBlock
		(Agens, swc:controlledBy, ?AGENTID)
interEffects
	add
		(Agens, swc:isOnTransitWay, Instrument) and
	del
		(Agens, swc:supportedBy, CurrLoc) and
effects
	add
		(Agens, swc:supportedBy, Target) and
	del
		(Agens, swc:isOnTransitWay, Instrument) and 
. % End of cycleFromTo


action
		driveToFrom(Agens, Patiens, Target, Instrument, [])
duration
		6
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Drive, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Instrument, swc:width, PathWidth) and
		(Agens, swc:width, AgensWidth) and
		(> PathWidth AgensWidth)
	nextBlock
		(Instrument, swc:fromGeographicArea, Target)
	nextBlock
		not(Agens, swc:attachedBy, SomeObjectA)
	nextBlock
		(Agens, swc:supportedBy, CurrLoc) and
		(Instrument, swc:toGeographicArea, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		not(Instrument, swc:hasDoor, TheDoor)
	nextBlock
		(Instrument, rdf:type, swc:GroundWay)
	nextBlock
		not(Instrument, swc:hasBlockingObstacle, SomeObstacle)
	nextBlock
		(Agens, swc:controlledBy, ?AGENTID)
interEffects
	add
		(Agens, swc:isOnTransitWay, Instrument) and
	del
		(Agens, swc:supportedBy, CurrLoc) and
effects
	add
		(Agens, swc:supportedBy, Target) and
	del
		(Agens, swc:isOnTransitWay, Instrument) and 
. % End of driveToFrom


action
		driveFromToDoor(Agens, Patiens, Target, Instrument, [])
duration
		6
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Drive, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Instrument, swc:width, PathWidth) and
		(Agens, swc:width, AgensWidth) and
		(> PathWidth AgensWidth)
	nextBlock
		(Instrument, swc:fromGeographicArea, CurrLoc) and
		(Agens, swc:supportedBy, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		not(Agens, swc:attachedBy, SomeObjectA)
	nextBlock
		(Instrument, rdf:type, swc:GroundWay)
	nextBlock
		(Instrument, swc:toGeographicArea, Target)
	nextBlock
		not(Instrument, swc:hasBlockingObstacle, SomeObstacle)
	nextBlock
		(Instrument, swc:hasDoor, TheDoor) and
		(DoorProperty, swc:isOpen, true) and
		(TheDoor, swc:hasOpenCloseProperties, DoorProperty)
	nextBlock
		(Agens, swc:controlledBy, ?AGENTID)
interEffects
	add
		(Agens, swc:isOnTransitWay, Instrument) and
	del
		(Agens, swc:supportedBy, CurrLoc) and
effects
	add
		(Agens, swc:supportedBy, Target) and
	del
		(Agens, swc:isOnTransitWay, Instrument) and 
. % End of driveFromToDoor


action
		driveFromTo(Agens, Patiens, Target, Instrument, [])
duration
		6
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Drive, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Instrument, swc:width, PathWidth) and
		(Agens, swc:width, AgensWidth) and
		(> PathWidth AgensWidth)
	nextBlock
		(Instrument, swc:fromGeographicArea, CurrLoc) and
		(Agens, swc:supportedBy, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		not(Agens, swc:attachedBy, SomeObjectA)
	nextBlock
		not(Instrument, swc:hasDoor, SomeDoor)
	nextBlock
		(Instrument, rdf:type, swc:GroundWay)
	nextBlock
		(Instrument, swc:toGeographicArea, Target)
	nextBlock
		not(Instrument, swc:hasBlockingObstacle, SomeObstacle)
	nextBlock
		(Agens, swc:controlledBy, ?AGENTID)
interEffects
	add
		(Agens, swc:isOnTransitWay, Instrument) and
	del
		(Agens, swc:supportedBy, CurrLoc) and
effects
	add
		(Agens, swc:supportedBy, Target) and
	del
		(Agens, swc:isOnTransitWay, Instrument) and 
. % End of driveFromTo




action
		flyToFromDoor(Agens, Patiens, Target, Instrument, [])
duration
		6
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Fly, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Instrument, rdf:type, swc:TransitWay)
	nextBlock
		(Instrument, swc:fromGeographicArea, Target)
	nextBlock
		not(Agens, swc:attachedBy, SomeObjectA)
	nextBlock
		(Agens, swc:supportedBy, CurrLoc) and
		(Instrument, swc:toGeographicArea, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		(Agens, swc:width, AgensWidth) and
		(Instrument, swc:flyingWidth, FlyingWidth) and
		(> FlyingWidth AgensWidth)
	nextBlock
		(Instrument, swc:hasDoor, TheDoor) and
		(DoorProperty, swc:isOpen, true) and
		(TheDoor, swc:hasOpenCloseProperties, DoorProperty)
	nextBlock
		(Agens, swc:controlledBy, ?AGENTID)
interEffects
	add
		(Agens, swc:isOnTransitWay, Instrument) and
	del
		(Agens, swc:supportedBy, CurrLoc) and
effects
	add
		(Agens, swc:supportedBy, Target) and
	del
		(Agens, swc:isOnTransitWay, Instrument) and 
. % End of flyToFromDoor


action
		cycleToFromDoor(Agens, Patiens, Target, Instrument, [])
duration
		6
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Cycle, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Instrument, swc:width, PathWidth) and
		(Agens, swc:width, AgensWidth) and
		(> PathWidth AgensWidth)
	nextBlock
		(Instrument, swc:fromGeographicArea, Target)
	nextBlock
		not(Agens, swc:attachedBy, SomeObjectA)
	nextBlock
		(Agens, swc:supportedBy, CurrLoc) and
		(Instrument, swc:toGeographicArea, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		(Instrument, rdf:type, swc:GroundWay)
	nextBlock
		not(Instrument, swc:hasBlockingObstacle, SomeObstacle)
	nextBlock
		(Instrument, swc:hasDoor, TheDoor) and
		(DoorProperty, swc:isOpen, true) and
		(TheDoor, swc:hasOpenCloseProperties, DoorProperty)
	nextBlock
		(Agens, swc:controlledBy, ?AGENTID)
interEffects
	add
		(Agens, swc:isOnTransitWay, Instrument) and
	del
		(Agens, swc:supportedBy, CurrLoc) and
effects
	add
		(Agens, swc:supportedBy, Target) and
	del
		(Agens, swc:isOnTransitWay, Instrument) and 
. % End of cycleToFromDoor


action
		runFromTo(Agens, Patiens, Target, Instrument, [])
duration
		6
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Run, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Instrument, swc:width, PathWidth) and
		(Agens, swc:width, AgensWidth) and
		(> PathWidth AgensWidth)
	nextBlock
		(Instrument, swc:fromGeographicArea, CurrLoc) and
		(Agens, swc:supportedBy, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		not(Agens, swc:attachedBy, SomeObjectA)
	nextBlock
		(Instrument, rdf:type, swc:GroundWay)
	nextBlock
		(Instrument, swc:toGeographicArea, Target)
	nextBlock
		not(Instrument, swc:hasBlockingObstacle, SomeObstacle)
	nextBlock
		not(Instrument, swc:hasDoor, ADoor)
	nextBlock
		(Agens, swc:controlledBy, ?AGENTID)
interEffects
	add
		(Agens, swc:isOnTransitWay, Instrument) and
	del
		(Agens, swc:supportedBy, CurrLoc) and
effects
	add
		(Agens, swc:supportedBy, Target) and
	del
		(Agens, swc:isOnTransitWay, Instrument) and 
. % End of runFromTo




action
		driveToFromDoor(Agens, Patiens, Target, Instrument, [])
duration
		6
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Drive, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Instrument, swc:width, PathWidth) and
		(Agens, swc:width, AgensWidth) and
		(> PathWidth AgensWidth)
	nextBlock
		(Instrument, swc:fromGeographicArea, Target)
	nextBlock
		not(Agens, swc:attachedBy, SomeObjectA)
	nextBlock
		(Agens, swc:supportedBy, CurrLoc) and
		(Instrument, swc:toGeographicArea, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		(Instrument, rdf:type, swc:GroundWay)
	nextBlock
		not(Instrument, swc:hasBlockingObstacle, SomeObstacle)
	nextBlock
		(Instrument, swc:hasDoor, TheDoor) and
		(DoorProperty, swc:isOpen, true) and
		(TheDoor, swc:hasOpenCloseProperties, DoorProperty)
	nextBlock
		(Agens, swc:controlledBy, ?AGENTID)
interEffects
	add
		(Agens, swc:isOnTransitWay, Instrument) and
	del
		(Agens, swc:supportedBy, CurrLoc) and
effects
	add
		(Agens, swc:supportedBy, Target) and
	del
		(Agens, swc:isOnTransitWay, Instrument) and 
. % End of driveToFromDoor


action
		flyFromToDoor(Agens, Patiens, Target, Instrument, [])
duration
		6
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Fly, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Instrument, rdf:type, swc:TransitWay)
	nextBlock
		(Instrument, swc:fromGeographicArea, CurrLoc) and
		(Agens, swc:supportedBy, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		not(Agens, swc:attachedBy, SomeObjectA)
	nextBlock
		(Agens, swc:width, AgensWidth) and
		(Instrument, swc:flyingWidth, FlyingWidth) and
		(> FlyingWidth AgensWidth)
	nextBlock
		(Instrument, swc:toGeographicArea, Target)
	nextBlock
		(Instrument, swc:hasDoor, TheDoor) and
		(DoorProperty, swc:isOpen, true) and
		(TheDoor, swc:hasOpenCloseProperties, DoorProperty)
	nextBlock
		(Agens, swc:controlledBy, ?AGENTID)
interEffects
	add
		(Agens, swc:isOnTransitWay, Instrument) and
	del
		(Agens, swc:supportedBy, CurrLoc) and
effects
	add
		(Agens, swc:supportedBy, Target) and
	del
		(Agens, swc:isOnTransitWay, Instrument) and 
. % End of flyFromToDoor


action
		runFromToDoor(Agens, Patiens, Target, Instrument, [])
duration
		6
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Run, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Instrument, swc:width, PathWidth) and
		(Agens, swc:width, AgensWidth) and
		(> PathWidth AgensWidth)
	nextBlock
		(Instrument, swc:fromGeographicArea, CurrLoc) and
		(Agens, swc:supportedBy, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		not(Agens, swc:attachedBy, SomeObjectA)
	nextBlock
		(Instrument, rdf:type, swc:GroundWay)
	nextBlock
		(Instrument, swc:toGeographicArea, Target)
	nextBlock
		not(Instrument, swc:hasBlockingObstacle, SomeObstacle)
	nextBlock
		(Instrument, swc:hasDoor, TheDoor) and
		(DoorProperty, swc:isOpen, true) and
		(TheDoor, swc:hasOpenCloseProperties, DoorProperty)
	nextBlock
		(Agens, swc:controlledBy, ?AGENTID)
interEffects
	add
		(Agens, swc:isOnTransitWay, Instrument) and
	del
		(Agens, swc:supportedBy, CurrLoc) and
effects
	add
		(Agens, swc:supportedBy, Target) and
	del
		(Agens, swc:isOnTransitWay, Instrument) and 
. % End of runFromToDoor




action
		cycleToFrom(Agens, Patiens, Target, Instrument, [])
duration
		6
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Cycle, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Instrument, swc:width, PathWidth) and
		(Agens, swc:width, AgensWidth) and
		(> PathWidth AgensWidth)
	nextBlock
		(Instrument, swc:fromGeographicArea, CurrLoc) and
		(Agens, swc:supportedBy, CurrLoc) and
		(CurrLoc, swc:isLowestLevel, true)
	nextBlock
		not(Agens, swc:attachedBy, SomeObjectA)
	nextBlock
		not(Instrument, swc:hasDoor, SomeDoor)
	nextBlock
		(Instrument, rdf:type, swc:GroundWay)
	nextBlock
		(Instrument, swc:toGeographicArea, Target)
	nextBlock
		not(Instrument, swc:hasBlockingObstacle, SomeObstacle)
	nextBlock
		(Agens, swc:controlledBy, ?AGENTID)
interEffects
	add
		(Agens, swc:isOnTransitWay, Instrument) and
	del
		(Agens, swc:supportedBy, CurrLoc) and
effects
	add
		(Agens, swc:supportedBy, Target) and
	del
		(Agens, swc:isOnTransitWay, Instrument) and 
. % End of cycleToFrom


*/
