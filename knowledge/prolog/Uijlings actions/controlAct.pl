action
		dropControlAction(Agens, Patiens, Target, Instrument, [])
duration
		1
interDuration
		1
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:DropControl, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swc:controlledBy, ControllingObject) and
		(ControlProperty, swc:controlledObject, Agens) and
		(ControllingObject, swc:hasControlProperties, ControlProperty) and
		(ControlProperty, swc:controlStrength, ControlStrength) and
		(ControlProperty, swc:controlType, ControlType)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
	del
		(ControlProperty, swc:controlledObject, Agens) and 
		(Agens, swc:controlledBy, ControllingObject) and 
		(ControlProperty, swc:controlStrength, ControlStrength) and 
		(ControlProperty, swc:controlType, ControlType) and 
. % End of dropControlAction


action
		takeControlContainedByNonSentient(Agens, Patiens, Target, Instrument, [])
duration
		3
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:TakeControl, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(= 100 ControlStrength)
	nextBlock
		(= "containedByNonSentient" ControlType)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Agens, swc:containedBy, Patiens)
	nextBlock
		(Agens, swc:hasControlProperties, ControlProperties)
	nextBlock
		(swc:hasControlType Patiens "containedByNonSentient")
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(Patiens, swc:controlledBy, Agens) and
		(ControlProperties, swc:controlType, ControlType) and
		(ControlProperties, swc:controlStrength, ControlStrength) and
		(ControlProperties, swc:controlledObject, Patiens) and
	del
. % End of takeControlContainedByNonSentient


action
		takeControlContainedBySentient(Agens, Patiens, Target, Instrument, [])
duration
		3
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Patiens, swc:needsControlSkill, NeededControlSkill) and
		(Agens, swc:controlledBy, aGENTID) and
		(Patiens, swc:controlDifficulty, ControlDifficultyPatiens) and
		(AgentControlSkill, rdf:type, NeededControlSkill) and
		(AgentControlSkill, swc:proficiency, ControlProficiency) and
		(aGENTID, swc:hasSkill, AgentControlSkill) and
		(- ControlProficiency ControlDifficultyPatiens ControlStrength) and
		(> ControlStrength 0)
	nextBlock
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:TakeControl, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Agens, swc:hasControlProperties, ControlProperties)
	nextBlock
		(Agens, swc:containedBy, Patiens)
	nextBlock
		(= "containedBySentient" ControlType)
	nextBlock
		(swc:hasControlType Patiens "containedBySentient")
interEffects
	add
	del
effects
	add
		(Patiens, swc:controlledBy, Agens) and
		(ControlProperties, swc:controlType, ControlType) and
		(ControlProperties, swc:controlStrength, ControlStrength) and
		(ControlProperties, swc:controlledObject, Patiens) and
	del
. % End of takeControlContainedBySentient


action
		takeControlSupportedByNonSentient(Agens, Patiens, Target, Instrument, [])
duration
		3
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:TakeControl, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(= 100 ControlStrength)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Agens, swc:hasControlProperties, ControlProperties)
	nextBlock
		(Agens, swc:supportedBy, Patiens)
	nextBlock
		(swc:hasControlType Patiens "supportedByNonSentient")
	nextBlock
		(= "supportedByNonSentient" ControlType)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(Patiens, swc:controlledBy, Agens) and
		(ControlProperties, swc:controlType, ControlType) and
		(ControlProperties, swc:controlStrength, ControlStrength) and
		(ControlProperties, swc:controlledObject, Patiens) and
	del
. % End of takeControlSupportedByNonSentient


action
		takeControlSupportedBySentient(Agens, Patiens, Target, Instrument, [])
duration
		3
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:TakeControl, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Patiens, swc:needsControlSkill, NeededControlSkill) and
		(Agens, swc:controlledBy, aGENTID) and
		(Patiens, swc:controlDifficulty, ControlDifficultyPatiens) and
		(AgentControlSkill, rdf:type, NeededControlSkill) and
		(AgentControlSkill, swc:proficiency, ControlProficiency) and
		(aGENTID, swc:hasSkill, AgentControlSkill) and
		(- ControlProficiency ControlDifficultyPatiens ControlStrength) and
		(> ControlStrength 0)
	nextBlock
		(= "supportedBySentient" ControlType)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Agens, swc:hasControlProperties, ControlProperties)
	nextBlock
		(swc:hasControlType Patiens "supportedBySentient")
	nextBlock
		(Agens, swc:supportedBy, Patiens)
interEffects
	add
	del
effects
	add
		(Patiens, swc:controlledBy, Agens) and
		(ControlProperties, swc:controlType, ControlType) and
		(ControlProperties, swc:controlStrength, ControlStrength) and
		(ControlProperties, swc:controlledObject, Patiens) and
	del
. % End of takeControlSupportedBySentient


