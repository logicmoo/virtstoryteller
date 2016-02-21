action
		tieRopeToObject(Agens, Patiens, Target, Instrument, [])
duration
		5
interDuration
		5
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Tie, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swcr:interactiveHeight, Target)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Patiens, swc:heldBy, Agens)
	nextBlock
		(Agens, swc:strength, StrengthAgens) and
		(- StrengthAgens 1 AttachingStrength)
	nextBlock
		(Patiens, rdf:type, swc:RopeLikeDevice)
	nextBlock
		(Patiens, swcr:availableRopeLength, AvailableRopeLengthTarget) and
		(Target, swcr:minCircumference, MinCircumferenceTarget) and
		(> AvailableRopeLengthTarget MinCircumferenceTarget)
	nextBlock
		(Patiens, swc:hasAttachedProperties, AttachedPropertiesPatiens) and
		(AttachedPropertiesPatiens, swc:attachingStrength, HoldingStrength)
	nextBlock
		(Agens, swcr:interactiveLocation, Target)
	nextBlock
		(Agens, swc:controlledBy, AgentID)
interEffects
	add
	del
effects
	add
		(Patiens, swc:attachedBy, Target) and
		(swc:attachingType AttachedPropertiesPatiens "knot") and
		(AttachedPropertiesPatiens, swc:attachingObject, Target) and
		(AttachedPropertiesPatiens, swc:attachingDevice, Patiens) and
		(AttachedPropertiesPatiens, swc:attachingStrength, AttachingStrength) and
	del
		(AttachedPropertiesPatiens, swc:attachingStrength, HoldingStrength) and 
		(AttachedPropertiesPatiens, swc:attachingDevice, Agens) and 
		(Patiens, swc:heldBy, Agens) and 
		(AttachedPropertiesPatiens, swc:attachingObject, Agens) and 
		(swc:attachingType AttachedPropertiesPatiens "grabbingDevice") and 
. % End of tieRopeToObject


action
		tieObjectToRope(Agens, Patiens, Target, Instrument, [])
duration
		5
interDuration
		5
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Tie, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swcr:interactiveHeight, Target)
	nextBlock
		(Patiens, swcr:minCircumference, MinCircumferencePatiens) and
		(Target, swcr:availableRopeLength, AvailableRopeLength) and
		(> AvailableRopeLength MinCircumferencePatiens)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Agens, swc:strength, StrengthAgens) and
		(- StrengthAgens 1 AttachingStrength)
	nextBlock
		(Patiens, swc:heldBy, Agens)
	nextBlock
		(Target, rdf:type, swc:RopeLikeDevice)
	nextBlock
		(Patiens, swc:hasAttachedProperties, AttachedPropertiesPatiens) and
		(AttachedPropertiesPatiens, swc:attachingStrength, HoldingStrength)
	nextBlock
		(Agens, swcr:interactiveLocation, Target)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(AttachedPropertiesPatiens, swc:attachingDevice, Target) and
		(Patiens, swc:attachedBy, Target) and
		(swc:attachingType AttachedPropertiesPatiens "knot") and
		(AttachedPropertiesPatiens, swc:attachingObject, Target) and
		(AttachedPropertiesPatiens, swc:attachingStrength, AttachingStrength) and
	del
		(AttachedPropertiesPatiens, swc:attachingStrength, HoldingStrength) and 
		(AttachedPropertiesPatiens, swc:attachingDevice, Agens) and 
		(Patiens, swc:heldBy, Agens) and 
		(AttachedPropertiesPatiens, swc:attachingObject, Agens) and 
		(swc:attachingType AttachedPropertiesPatiens "grabbingDevice") and 
. % End of tieObjectToRope


action
		untieRopeFromObject(Agens, Patiens, Target, Instrument, [])
duration
		5
interDuration
		5
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Untie, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swc:strength, StrengthAgens) and
		(AttachedProperties, swc:attachingStrength, AttachingStrength) and
		(> StrengthAgens AttachingStrength)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Patiens, swc:attachedBy, Target)
	nextBlock
		(Patiens, swc:hasAttachedProperties, AttachedPropertiesPatiens) and
		(swc:attachingType AttachedPropertiesPatiens "knot")
	nextBlock
		not(HeldObj, swc:heldBy, Agens)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(Patiens, swc:supportedBy, Target) and
	del
		(Patiens, swc:attachedBy, Target) and 
		(swc:attachingType AttachedPropertiesPatiens "knot") and 
		(AttachedPropertiesPatiens, swc:attachingObject, Target) and 
		(AttachedPropertiesPatiens, swc:attachingDevice, Patiens) and 
		(AttachedPropertiesPatiens, swc:attachingStrength, AttachingStrength) and 
. % End of untieRopeFromObject


