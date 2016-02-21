action
		openDoorNoLock(Agens, Patiens, Target, Instrument, [])
duration
		1
interDuration
		1
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Open, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swcr:atLocation, CurrLoc) and
		(Agens, swc:supportedBy, CurrLoc) and
		(Path, swc:hasDoor, Patiens) and
		(Path, swc:length, 1) and
		(Path, swc:connectedToGeographicArea, CurrLoc)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Patiens, swc:hasOpenCloseProperties, OpenCloseProperties) and
		(OpenCloseProperties, swc:isOpenable, true) and
		(OpenCloseProperties, swc:isOpen, false) and
		not(OpenCloseProperties, swc:hasLock, SomeLock)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(OpenCloseProperties, swc:isOpen, true) and
	del
		(OpenCloseProperties, swc:isOpen, false) and 
. % End of openDoorNoLock


action
		openContainerWithLock(Agens, Patiens, Target, Instrument, [])
duration
		1
interDuration
		1
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Open, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Patiens, swc:hasOpenCloseProperties, OpenCloseProperties) and
		(OpenCloseProperties, swc:isOpenable, true) and
		(OpenCloseProperties, swc:isOpen, false) and
		(TheLock, swc:isLocked, false) and
		(OpenCloseProperties, swc:hasLock, TheLock)
	nextBlock
		(Agens, swcr:interactiveLocation, Patiens)
	nextBlock
		(Agens, swcr:interactiveHeight, Patiens)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(OpenCloseProperties, swc:isOpen, true) and
	del
		(OpenCloseProperties, swc:isOpen, false) and 
. % End of openContainerWithLock


action
		lockContainerWithCode(Agens, Patiens, Target, Instrument, [])
duration
		2
interDuration
		2
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Lock, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Patiens, swc:hasOpenCloseProperties, OpenCloseProperties) and
		(TheLock, swc:isLocked, false) and
		(swc:lockType TheLock "code") and
		(OpenCloseProperties, swc:hasLock, TheLock)
	nextBlock
		(Agens, swcr:interactiveHeight, Patiens)
	nextBlock
		(Agens, swcr:interactiveLocation, Patiens)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(TheLock, swc:isLocked, true) and
	del
		(TheLock, swc:isLocked, false) and 
. % End of lockContainerWithCode


action
		unlockDoorWithKey(Agens, Patiens, Target, Instrument, [])
duration
		2
interDuration
		2
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Unlock, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swcr:atLocation, CurrLoc) and
		(Agens, swc:supportedBy, CurrLoc) and
		(Path, swc:hasDoor, Patiens) and
		(Path, swc:length, 1) and
		(Path, swc:connectedToGeographicArea, CurrLoc)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(TheLock, swc:physicalKey, Instrument) and
		(Patiens, swc:hasOpenCloseProperties, OpenCloseProperties) and
		(swc:lockType TheLock "physicalKey") and
		(TheLock, swc:isLocked, true) and
		(OpenCloseProperties, swc:hasLock, TheLock)
	nextBlock
		(Instrument, swc:heldBy, Agens)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(TheLock, swc:isLocked, false) and
	del
		(TheLock, swc:isLocked, true) and 
. % End of unlockDoorWithKey


action
		unlockDoorWithCode(Agens, Patiens, Target, Instrument, [])
duration
		2
interDuration
		2
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Unlock, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swcr:atLocation, CurrLoc) and
		(Agens, swc:supportedBy, CurrLoc) and
		(Path, swc:hasDoor, Patiens) and
		(Path, swc:length, 1) and
		(Path, swc:connectedToGeographicArea, CurrLoc)
	nextBlock
		(TheLock, swc:code, Instrument) and
		(Patiens, swc:hasOpenCloseProperties, OpenCloseProperties) and
		(TheLock, swc:isLocked, true) and
		(swc:lockType TheLock "code") and
		(OpenCloseProperties, swc:hasLock, TheLock)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(TheLock, swc:isLocked, false) and
	del
		(TheLock, swc:isLocked, true) and 
. % End of unlockDoorWithCode


action
		closeContainerNoLock(Agens, Patiens, Target, Instrument, [])
duration
		1
interDuration
		1
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Close, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Patiens, swc:hasOpenCloseProperties, OpenCloseProperties) and
		(OpenCloseProperties, swc:isOpen, true) and
		(OpenCloseProperties, swc:isOpenable, true) and
		not(OpenCloseProperties, swc:hasLock, SomeLock)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Patiens, rdf:type, swc:Container)
	nextBlock
		(Agens, swcr:interactiveHeight, Patiens)
	nextBlock
		(Agens, swcr:interactiveLocation, Patiens)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(OpenCloseProperties, swc:isOpen, false) and
	del
		(OpenCloseProperties, swc:isOpen, true) and 
. % End of closeContainerNoLock


action
		foldAction(Agens, Patiens, Target, Instrument, [])
duration
		3
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Fold, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Patiens, swc:supportedBy, SupportingObject)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Patiens, swc:foldableInto, FoldedObject)
	nextBlock
		(Agens, swcr:interactiveLocation, Patiens)
	nextBlock
		(Agens, swcr:interactiveHeight, Patiens)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(FoldedObject, swc:supportedBy, SupportingObject) and
	del
		(Patiens, swc:supportedBy, SupportingObject) and 
. % End of foldAction


action
		lockContainerWithKey(Agens, Patiens, Target, Instrument, [])
duration
		2
interDuration
		2
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Lock, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(TheLock, swc:physicalKey, Instrument) and
		(Patiens, swc:hasOpenCloseProperties, OpenCloseProperties) and
		(swc:lockType TheLock "physicalKey") and
		(TheLock, swc:isLocked, false) and
		(OpenCloseProperties, swc:hasLock, TheLock)
	nextBlock
		(Instrument, swc:heldBy, Agens)
	nextBlock
		(Agens, swcr:interactiveLocation, Patiens)
	nextBlock
		(Agens, swcr:interactiveHeight, Patiens)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(TheLock, swc:isLocked, true) and
	del
		(TheLock, swc:isLocked, false) and 
. % End of lockContainerWithKey


action
		openDoorWithLock(Agens, Patiens, Target, Instrument, [])
duration
		1
interDuration
		1
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Open, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swcr:atLocation, CurrLoc) and
		(Agens, swc:supportedBy, CurrLoc) and
		(Path, swc:hasDoor, Patiens) and
		(Path, swc:length, 1) and
		(Path, swc:connectedToGeographicArea, CurrLoc)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Patiens, swc:hasOpenCloseProperties, OpenCloseProperties) and
		(OpenCloseProperties, swc:isOpenable, true) and
		(OpenCloseProperties, swc:isOpen, false) and
		(TheLock, swc:isLocked, false) and
		(OpenCloseProperties, swc:hasLock, TheLock)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(OpenCloseProperties, swc:isOpen, true) and
	del
		(OpenCloseProperties, swc:isOpen, false) and 
. % End of openDoorWithLock


action
		closeDoorWithLock(Agens, Patiens, Target, Instrument, [])
duration
		1
interDuration
		1
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Close, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swcr:atLocation, CurrLoc) and
		(Agens, swc:supportedBy, CurrLoc) and
		(Path, swc:hasDoor, Patiens) and
		(Path, swc:length, 1) and
		(Path, swc:connectedToGeographicArea, CurrLoc)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Patiens, swc:hasOpenCloseProperties, OpenCloseProperties) and
		(OpenCloseProperties, swc:isOpen, true) and
		(OpenCloseProperties, swc:isOpenable, true) and
		(OpenCloseProperties, swc:hasLock, ALock)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(OpenCloseProperties, swc:isOpen, false) and
	del
		(OpenCloseProperties, swc:isOpen, true) and 
. % End of closeDoorWithLock


action
		closeDoorNoLock(Agens, Patiens, Target, Instrument, [])
duration
		1
interDuration
		1
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Close, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swcr:atLocation, CurrLoc) and
		(Agens, swc:supportedBy, CurrLoc) and
		(Path, swc:hasDoor, Patiens) and
		(Path, swc:length, 1) and
		(Path, swc:connectedToGeographicArea, CurrLoc)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Patiens, swc:hasOpenCloseProperties, OpenCloseProperties) and
		(OpenCloseProperties, swc:isOpen, true) and
		(OpenCloseProperties, swc:isOpenable, true) and
		not(OpenCloseProperties, swc:hasLock, SomeLock)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(OpenCloseProperties, swc:isOpen, false) and
	del
		(OpenCloseProperties, swc:isOpen, true) and 
. % End of closeDoorNoLock


action
		closeContainerWithLock(Agens, Patiens, Target, Instrument, [])
duration
		1
interDuration
		1
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Close, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Patiens, swc:hasOpenCloseProperties, OpenCloseProperties) and
		(OpenCloseProperties, swc:isOpen, true) and
		(OpenCloseProperties, swc:isOpenable, true) and
		(OpenCloseProperties, swc:hasLock, ALock)
	nextBlock
		(Agens, swcr:interactiveLocation, Patiens)
	nextBlock
		(Agens, swcr:interactiveHeight, Patiens)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(OpenCloseProperties, swc:isOpen, false) and
	del
		(OpenCloseProperties, swc:isOpen, true) and 
. % End of closeContainerWithLock


action
		unlockContainerWithCode(Agens, Patiens, Target, Instrument, [])
duration
		2
interDuration
		2
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Unlock, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(TheLock, swc:code, Instrument) and
		(Patiens, swc:hasOpenCloseProperties, OpenCloseProperties) and
		(TheLock, swc:isLocked, true) and
		(swc:lockType TheLock "code") and
		(OpenCloseProperties, swc:hasLock, TheLock)
	nextBlock
		(Agens, swcr:interactiveHeight, Patiens)
	nextBlock
		(Agens, swcr:interactiveLocation, Patiens)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(TheLock, swc:isLocked, false) and
	del
		(TheLock, swc:isLocked, true) and 
. % End of unlockContainerWithCode


action
		lockDoorWithCode(Agens, Patiens, Target, Instrument, [])
duration
		2
interDuration
		2
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Lock, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swcr:atLocation, CurrLoc) and
		(Agens, swc:supportedBy, CurrLoc) and
		(Path, swc:hasDoor, Patiens) and
		(Path, swc:length, 1) and
		(Path, swc:connectedToGeographicArea, CurrLoc)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Patiens, swc:hasOpenCloseProperties, OpenCloseProperties) and
		(TheLock, swc:isLocked, false) and
		(swc:lockType TheLock "code") and
		(OpenCloseProperties, swc:hasLock, TheLock)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(TheLock, swc:isLocked, true) and
	del
		(TheLock, swc:isLocked, false) and 
. % End of lockDoorWithCode


action
		unlockContainerWithKey(Agens, Patiens, Target, Instrument, [])
duration
		2
interDuration
		2
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Unlock, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(TheLock, swc:physicalKey, Instrument) and
		(Patiens, swc:hasOpenCloseProperties, OpenCloseProperties) and
		(swc:lockType TheLock "physicalKey") and
		(TheLock, swc:isLocked, true) and
		(OpenCloseProperties, swc:hasLock, TheLock)
	nextBlock
		(Agens, swcr:interactiveHeight, Patiens)
	nextBlock
		(Agens, swcr:interactiveLocation, Patiens)
	nextBlock
		(Instrument, swc:heldBy, Agens)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(TheLock, swc:isLocked, false) and
	del
		(TheLock, swc:isLocked, true) and 
. % End of unlockContainerWithKey


action
		openContainerNoLock(Agens, Patiens, Target, Instrument, [])
duration
		1
interDuration
		1
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Open, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Patiens, swc:hasOpenCloseProperties, OpenCloseProperties) and
		(OpenCloseProperties, swc:isOpenable, true) and
		(OpenCloseProperties, swc:isOpen, false) and
		not(OpenCloseProperties, swc:hasLock, SomeLock)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Patiens, rdf:type, swc:Container)
	nextBlock
		(Agens, swcr:interactiveLocation, Patiens)
	nextBlock
		(Agens, swcr:interactiveHeight, Patiens)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(OpenCloseProperties, swc:isOpen, true) and
	del
		(OpenCloseProperties, swc:isOpen, false) and 
. % End of openContainerNoLock


action
		lockDoorWithKey(Agens, Patiens, Target, Instrument, [])
duration
		2
interDuration
		2
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Lock, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swcr:atLocation, CurrLoc) and
		(Agens, swc:supportedBy, CurrLoc) and
		(Path, swc:hasDoor, Patiens) and
		(Path, swc:length, 1) and
		(Path, swc:connectedToGeographicArea, CurrLoc)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(TheLock, swc:physicalKey, Instrument) and
		(Patiens, swc:hasOpenCloseProperties, OpenCloseProperties) and
		(swc:lockType TheLock "physicalKey") and
		(TheLock, swc:isLocked, false) and
		(OpenCloseProperties, swc:hasLock, TheLock)
	nextBlock
		(Instrument, swc:heldBy, Agens)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(TheLock, swc:isLocked, true) and
	del
		(TheLock, swc:isLocked, false) and 
. % End of lockDoorWithKey


