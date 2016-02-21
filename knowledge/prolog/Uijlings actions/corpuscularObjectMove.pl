action
		moveOutContainerOnTarget(Agens, Patiens, Target, Instrument, [])
duration
		2
interDuration
		2
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:MoveOut, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swcr:movableHeight, Target)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Agens, swc:containedBy, ThisContainer) and
		(OpenCloseProperties, swc:isOpen, true) and
		(ThisContainer, rdf:type, swc:Container) and
		(ThisContainer, swc:hasOpenCloseProperties, OpenCloseProperties)
	nextBlock
		(Agens, swcr:interactiveLocation, Target)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(Agens, swc:supportedBy, Target) and
	del
		(Agens, swc:containedBy, ThisContainer) and 
. % End of moveOutContainerOnTarget


action
		moveInContainer(Agens, Patiens, Target, Instrument, [])
duration
		4
interDuration
		4
superAction
		TOBEINSERTED
preconditions
		(Agens, swcr:stapleVolume, VolumeAgens) and
		(Target, swcr:availableCapacity, AvailableCapacityTarget) and
		(> AvailableCapacityTarget VolumeAgens)
	nextBlock
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:MoveIn, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swcr:interactiveHeight, Target)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Target, swc:hasOpenCloseProperties, OpenCloseProperties) and
		(OpenCloseProperties, swc:isOpen, true)
	nextBlock
		(Target, rdf:type, swc:Container)
	nextBlock
		(Agens, swc:supportedBy, SupportingObject)
	nextBlock
		(Agens, swcr:interactiveLocation, Target)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(Agens, swc:containedBy, Target) and
	del
		(Agens, swc:supportedBy, SupportingObject) and 
. % End of moveInContainer


action
		stepOnTarget(Agens, Patiens, Target, Instrument, [])
duration
		2
interDuration
		2
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:StepOn, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swcr:movableHeight, Target)
	nextBlock
		(Agens, swcr:validTransferLocation, Target)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Agens, swc:supportedBy, SupportingObject)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(Agens, swc:supportedBy, Target) and
	del
		(Agens, swc:supportedBy, SupportingObject) and 
. % End of stepOnTarget


