action
		eatHealthFoodNotMaxHealth(Agens, Patiens, Target, Instrument, [])
duration
		5
interDuration
		5
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Eat, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swc:health, HealthAgens) and
		(Agens, swc:maxHealth, MaxHealthAgens) and
		(Patiens, swc:containsBioActiveSubstance, BioActiveSubstance) and
		(swc:affectsBodyAttribute BioActiveSubstance "swc:health") and
		(BioActiveSubstance, swc:potency, Potency) and
		(+ HealthAgens Potency ProposedHealth) and
		(= ProposedHealth NewHealth) and
		(< ProposedHealth MaxHealthAgens)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Patiens, swc:heldBy, Agens)
	nextBlock
		(Patiens, swc:hasAttachedProperties, AttachedPropertiesPatiens) and
		(AttachedPropertiesPatiens, swc:attachingStrength, AttachingStrength)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(Agens, swc:health, NewHealth) and
	del
		(AttachedPropertiesPatiens, swc:attachingDevice, Agens) and 
		(Patiens, swc:heldBy, Agens) and 
		(AttachedPropertiesPatiens, swc:attachingObject, Agens) and 
		(swc:attachingType AttachedPropertiesPatiens "grabbingDevice") and 
		(AttachedPropertiesPatiens, swc:attachingStrength, AttachingStrength) and 
. % End of eatHealthFoodNotMaxHealth


action
		eatHealthFoodMaxHealth(Agens, Patiens, Target, Instrument, [])
duration
		5
interDuration
		5
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Eat, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swc:health, HealthAgens) and
		(Agens, swc:maxHealth, MaxHealthAgens) and
		(Patiens, swc:containsBioActiveSubstance, BioActiveSubstance) and
		(swc:affectsBodyAttribute BioActiveSubstance "swc:health") and
		(BioActiveSubstance, swc:potency, Potency) and
		(+ HealthAgens Potency ProposedHealth) and
		(> ProposedHealth MaxHealthAgens) and
		(= MaxHealthAgens NewHealth)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Patiens, swc:heldBy, Agens)
	nextBlock
		(Patiens, swc:hasAttachedProperties, AttachedPropertiesPatiens) and
		(AttachedPropertiesPatiens, swc:attachingStrength, AttachingStrength)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(Agens, swc:health, NewHealth) and
	del
		(AttachedPropertiesPatiens, swc:attachingDevice, Agens) and 
		(Patiens, swc:heldBy, Agens) and 
		(AttachedPropertiesPatiens, swc:attachingObject, Agens) and 
		(swc:attachingType AttachedPropertiesPatiens "grabbingDevice") and 
		(AttachedPropertiesPatiens, swc:attachingStrength, AttachingStrength) and 
. % End of eatHealthFoodMaxHealth


action
		eatHealthFoodEqualHealth(Agens, Patiens, Target, Instrument, [])
duration
		5
interDuration
		5
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Eat, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swc:health, HealthAgens) and
		(Agens, swc:maxHealth, MaxHealthAgens) and
		(Patiens, swc:containsBioActiveSubstance, BioActiveSubstance) and
		(swc:affectsBodyAttribute BioActiveSubstance "swc:health") and
		(BioActiveSubstance, swc:potency, Potency) and
		(+ HealthAgens Potency ProposedHealth) and
		(= ProposedHealth NewHealth) and
		(= ProposedHealth MaxHealthAgens)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Patiens, swc:heldBy, Agens)
	nextBlock
		(Patiens, swc:hasAttachedProperties, AttachedPropertiesPatiens) and
		(AttachedPropertiesPatiens, swc:attachingStrength, AttachingStrength)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(Agens, swc:health, NewHealth) and
	del
		(AttachedPropertiesPatiens, swc:attachingDevice, Agens) and 
		(Patiens, swc:heldBy, Agens) and 
		(AttachedPropertiesPatiens, swc:attachingObject, Agens) and 
		(swc:attachingType AttachedPropertiesPatiens "grabbingDevice") and 
		(AttachedPropertiesPatiens, swc:attachingStrength, AttachingStrength) and 
. % End of eatHealthFoodEqualHealth


