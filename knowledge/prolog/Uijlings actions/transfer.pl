%%%
% New version by Edze Kruizinga
% @date 26 jan 2007
% added ( ) around action to make the action name available as a variable value

% transfer
% 	take
% 		takeFrom
%		undress
%		(extract
% 			takeOut)
% 	put
%		putOn
%		dress
%		(insert
%			putIn)

%%% Transfer
action
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Transfer', (AgentID, Agens, Patiens, Target, Instrument, [Vars, CurrLoc]))
superAction
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Action', (AgentID, Agens, Patiens, Target, Instrument, Vars))
primitiveAction
		false
preconditions
		% Agens is no Patiens
		math(Agens \= Patiens)
	nextBlock
		% Agens and Patiens must be at the same location
		(Agens, swcr:atLocation, CurrLoc) and
		(Patiens, swcr:atLocation, CurrLoc)
.

%%% Take
action
		%('http://www.owl-ontologies.com/FabulaKnowledge.owl#Take', (AgentID, Agens, Patiens, Target, _Instrument, [Vars, AttachedPropertiesPatiens, StrengthAgens]))
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Take', (AgentID, Agens, Patiens, Target, Instrument, [Vars, AttachedPropertiesPatiens]))
superAction
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Transfer', (AgentID, Agens, Patiens, Target, Instrument, Vars))
primitiveAction
		false
preconditions
		% Patiens is not attached by an object
		unp (Patiens, swc:attachedBy, _SomeObjectA)
	nextBlock
		% The agens does not hold any objects
		unp (_SomeObjectB, swc:heldBy, Agens)
	% nextBlock
		% Get Strength variable
		% (Agens, swc:strength, StrengthAgens) and
		% (Patiens, swcr:stapleWeight, WeightPatiens) and
		% math(StrengthAgens > WeightPatiens)
	nextBlock
		% Get AttachedProperties of the Patiens
		(Patiens, swc:hasAttachedProperties, AttachedPropertiesPatiens)
effects
	add
		% The Agens holds the target. 
		% Also update holding properties
		(Patiens, swc:heldBy, Agens) and
		(AttachedPropertiesPatiens, swc:attachingDevice, Agens) and
		(AttachedPropertiesPatiens, swc:attachingObject, Agens) % and
		% Edze: disabled as a hack to make it work
		% (AttachedPropertiesPatiens, swc:attachingStrength, StrengthAgens) and
		% (AttachedPropertiesPatiens, swc:attachingType, "grabbingDevice")
	del
		none
.


%%% TakeFrom
action
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#TakeFrom', (AgentID, Agens, Patiens, Target, Instrument, Vars))
superAction
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Take', (AgentID, Agens, Patiens, Target, Instrument, Vars))
primitiveAction
		true
duration
		2
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:'TakeFrom', owlr:classOrSubClassOf, TheAgensAction)
	nextBlock
		(Patiens, swc:supportedBy, Target)
effects
	add
		none
	del
		(Patiens, swc:supportedBy, Target) 
. % End of takeFrom


%%% Undress
action
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Undress', (AgentID, Agens, Patiens, Target, Instrument, Vars))
superAction
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Take', (AgentID, Agens, Patiens, Target, Instrument, Vars))
primitiveAction
		true
duration
		6
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:'Undress', owlr:classOrSubClassOf, TheAgensAction)
	nextBlock
		(Patiens, swc:wornBy, Agens)
effects
	add
		none
	del
		(Patiens, swc:wornBy, Agens)
. % End of undress


%%% Put
action
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Put', (AgentID, Agens, Patiens, Target, Instrument, [Vars, AttachedPropertiesPatiens]))
superAction
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Transfer', (AgentID, Agens, Patiens, Target, Instrument, Vars))
primitiveAction
		false
preconditions
		(Patiens, swc:heldBy, Agens)
	nextBlock
		(Patiens, swc:hasAttachedProperties, AttachedPropertiesPatiens) % and
		% (AttachedPropertiesPatiens, swc:attachingStrength, AttachingStrength)
effects
	add
		none
	del
		(Patiens, swc:heldBy, Agens) and 
		(AttachedPropertiesPatiens, swc:attachingDevice, Agens) and 
		(AttachedPropertiesPatiens, swc:attachingObject, Agens) % and 
		% (AttachedPropertiesPatiens, swc:attachingStrength, AttachingStrength) and
		% (AttachedPropertiesPatiens, swc:attachingType, "grabbingDevice")
. % End of put

%%% PutOn
action
		%(putOn, (AgentID, Agens, Patiens, Target, Instrument, Vars))
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#PutOn', (AgentID, Agens, Patiens, Target, null, Vars))
superAction
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Put', (AgentID, Agens, Patiens, Target, null, Vars))
primitiveAction
		true
duration
		2
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:'PutOn', owlr:classOrSubClassOf, TheAgensAction)
	% nextBlock
		%(Patiens, swc:height, HeightPatiens) and
		%(Target, swcr:totalHeight, TotalHeightTarget) and
		% (Agens, swcr:distanceToGround, DistanceToGroundAgens) and
		%(Agens, swcr:totalHeight, TotalHeightAgens) and
		% (TotalHeightTarget is HeightPatiens + ProjectedTotalHeightObject) and
		% math(ProjectedTotalHeightObject > DistanceToGroundAgens) and
		% math(TotalHeightAgens > TotalHeightTarget)
	nextBlock
		(Agens, swcr:validTransferLocation, Target)
effects
	add
		(Patiens, swc:supportedBy, Target)
	del
		none
. % End of putOn


%%% Dress
action
		%('http://www.owl-ontologies.com/FabulaKnowledge.owl#Dress', (AgentID, Agens, Patiens, Target, Instrument, Vars))
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Dress', (AgentID, Agens, Patiens, null, null, Vars)) % kruizinga, trying 'null'
superAction
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Put', (AgentID, Agens, Patiens, null, null, Vars))
primitiveAction
		true
duration
		5
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:'Dress', owlr:classOrSubClassOf, TheAgensAction)
	nextBlock
		(Patiens, owlr:typeOrSubType, swc:'WearableItem')
effects
	add
		(Patiens, swc:wornBy, Agens)
	del
		none
. % End of dress

/*

action
		takeOutContainer(Agens, Patiens, Target, Instrument, [])
duration
		2
interDuration
		2
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:TakeOut, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Agens, swc:strength, StrengthAgens) and
		(Patiens, swcr:stapleWeight, WeightPatiens) and
		(> StrengthAgens WeightPatiens)
	nextBlock
		(Patiens, swc:containedBy, ThisContainer) and
		(OpenCloseProperties, swc:isOpen, true) and
		(ThisContainer, swc:hasOpenCloseProperties, OpenCloseProperties)
	nextBlock
		(Patiens, swcr:interactiveHeight, Agens)
	nextBlock
		(unp (= Agens Patiens))
	nextBlock
		(Agens, swcr:interactiveLocation, Patiens)
	nextBlock
		not(Patiens, swc:attachedBy, AttachingObject)
	nextBlock
		(Patiens, swc:hasAttachedProperties, AttachedPropertiesPatiens)
	nextBlock
		not(SomeObject, swc:heldBy, Agens)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(AttachedPropertiesPatiens, swc:attachingDevice, Agens) and
		(Patiens, swc:heldBy, Agens) and
		(AttachedPropertiesPatiens, swc:attachingObject, Agens) and
		(AttachedPropertiesPatiens, swc:attachingStrength, StrengthAgens) and
		(swc:attachingType AttachedPropertiesPatiens "grabbingDevice") and
	del
		(Patiens, swc:containedBy, ThisContainer) and 
. % End of takeOutContainer


action
		putInContainer(Agens, Patiens, Target, Instrument, [])
duration
		2
interDuration
		2
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:PutIn, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swcr:interactiveHeight, Target)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Target, swc:hasOpenCloseProperties, OpenCloseProperties) and
		(OpenCloseProperties, swc:isOpen, true)
	nextBlock
		(Patiens, swc:heldBy, Agens)
	nextBlock
		(Target, swcr:availableCapacity, AvailableCapacityTarget) and
		(Patiens, swcr:stapleVolume, VolumePatiens) and
		(> AvailableCapacityTarget VolumePatiens)
	nextBlock
		(unp (= Agens Patiens))
	nextBlock
		(Target, rdf:type, swc:Container)
	nextBlock
		(Agens, swcr:interactiveLocation, Target)
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
		(Patiens, swc:containedBy, Target) and
	del
		(AttachedPropertiesPatiens, swc:attachingDevice, Agens) and 
		(Patiens, swc:heldBy, Agens) and 
		(AttachedPropertiesPatiens, swc:attachingObject, Agens) and 
		(swc:attachingType AttachedPropertiesPatiens "grabbingDevice") and 
		(AttachedPropertiesPatiens, swc:attachingStrength, AttachingStrength) and 
. % End of putInContainer


*/

