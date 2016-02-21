action
		disassembleObjectFromHeap(Agens, Patiens, Target, Instrument, [])
duration
		1
interDuration
		1
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Disassemble, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swcr:interactiveHeight, Target)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Agens, swcr:interactiveLocation, Target)
	nextBlock
		(Target, swc:supportedBy, SupportingObject)
	nextBlock
		(Target, rdf:type, swc:ConstructionHeap)
	nextBlock
		(Patiens, swc:partOfCorpuscularObject, Target)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(Patiens, swc:supportedBy, SupportingObject) and
	del
		(Patiens, swc:partOfCorpuscularObject, Target) and 
. % End of disassembleObjectFromHeap


action
		disassembleHeap(Agens, Patiens, Target, Instrument, [])
duration
		1
interDuration
		1
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Disassemble, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Patiens, swc:supportedBy, SupportingObject)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		not(SomeObject, swc:partOfCorpuscularObject, Patiens)
	nextBlock
		(Agens, swcr:interactiveHeight, Patiens)
	nextBlock
		(Agens, swcr:interactiveLocation, Patiens)
	nextBlock
		(Patiens, rdf:type, swc:ConstructionHeap)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
	del
		(Patiens, swc:supportedBy, SupportingObject) and 
		(Patiens, rdf:type, swc:ConstructionHeap) and 
. % End of disassembleHeap


action
		assignObjectToHeap(Agens, Patiens, Target, Instrument, [])
duration
		1
interDuration
		1
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Assemble, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Agens, swcr:interactiveHeight, Target)
	nextBlock
		not(Patiens, rdf:type, swc:ConstructionHeap)
	nextBlock
		(Patiens, swc:supportedBy, SupportingObject) and
		(Target, swc:supportedBy, SupportingObject)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Agens, swcr:interactiveLocation, Target)
	nextBlock
		(Target, rdf:type, swc:ConstructionHeap)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
interEffects
	add
	del
effects
	add
		(Patiens, swc:partOfCorpuscularObject, Target) and
	del
		(Patiens, swc:supportedBy, SupportingObject) and 
. % End of assignObjectToHeap


action
		assembleObjectFromHeap(Agens, Patiens, Target, Instrument, [])
duration
		10
interDuration
		10
superAction
		TOBEINSERTED
preconditions
		(Prototype, rdf:type, Target) and
		(Patiens, swcr:hasNecessaryMaterials, Prototype) and
		(Agens, swc:controlledBy, aGENTID) and
		(Prototype, swc:locatedAtPrototypeStorage, swc:ThePrototypeStorage) and
		(Prototype, swc:height, HeightPrototype) and
		(Prototype, swc:needsCreationSkill, NeededCreationSkill) and
		(Prototype, swc:length, LengthPrototype) and
		(Prototype, swc:toughness, ToughnessPrototype) and
		(SkillAgent, rdf:type, NeededCreationSkill) and
		(Prototype, swc:width, WidthPrototype) and
		(Prototype, swc:weight, WeightPrototype) and
		(Prototype, swc:creationDifficulty, CreationDifficulty) and
		(aGENTID, swc:hasSkill, SkillAgent) and
		(SkillAgent, swc:proficiency, ProficiencyAgent) and
		(> ProficiencyAgent CreationDifficulty)
	nextBlock
		(Patiens, swc:length, PatiensLength)
	nextBlock
		(Agens, swcr:interactiveLocation, Patiens)
	nextBlock
		(Patiens, rdf:type, swc:ConstructionHeap)
	nextBlock
		not(SomeSupportedObject, swc:supportedBy, Patiens)
	nextBlock
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Assemble, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Patiens, swc:height, PatiensHeight)
	nextBlock
		(Patiens, swc:weight, PatiensWeight)
	nextBlock
		(Patiens, swc:width, PatiensWidth)
	nextBlock
		(Agens, swcr:interactiveHeight, Patiens)
interEffects
	add
	del
effects
	add
		(Patiens, swc:weight, WeightPrototype) and
		(Patiens, swc:width, WidthPrototype) and
		(Patiens, swc:length, LengthPrototype) and
		(Patiens, rdf:type, Target) and
		(Patiens, swc:toughness, ToughnessPrototype) and
		(Patiens, swc:height, HeightPrototype) and
		(Patiens, swc:creationDifficulty, CreationDifficulty) and
	del
		(Patiens, swc:length, PatiensLength) and 
		(Patiens, swc:width, PatiensWidth) and 
		(Patiens, swc:weight, PatiensWeight) and 
		(Patiens, swc:height, PatiensHeight) and 
		(Patiens, rdf:type, swc:ConstructionHeap) and 
. % End of assembleObjectFromHeap


action
		disassembleObjectToConstructionHeap(Agens, Patiens, Target, Instrument, [])
duration
		10
interDuration
		10
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Disassemble, rdfs:subClassOf, TheAgensAction)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		(Agens, swcr:interactiveLocation, Patiens)
	nextBlock
		(Patiens, swc:needsCreationSkill, CreationSkillNeeded) and
		(Patiens, swc:creationDifficulty, CreationDifficultyPatiens) and
		(Agens, swc:controlledBy, aGENTID) and
		(SkillAgens, rdf:type, CreationSkillNeeded) and
		(SkillAgens, swc:proficiency, ProficiencyAgens) and
		(aGENTID, swc:hasSkill, SkillAgens) and
		(> ProficiencyAgens CreationDifficultyPatiens)
	nextBlock
		(Patiens, swc:supportedBy, SupportingObject)
	nextBlock
		(= Instrument PatiensDirectClass)
	nextBlock
		(Patiens, rdf:type, Instrument)
	nextBlock
		(Agens, swcr:interactiveHeight, Patiens)
interEffects
	add
	del
effects
	add
		(Patiens, rdf:type, swc:ConstructionHeap) and
	del
		(Patiens, rdf:type, PatiensDirectClass) and 
. % End of disassembleObjectToConstructionHeap


action
		createHeap(Agens, Patiens, Target, Instrument, [])
duration
		3
interDuration
		3
superAction
		TOBEINSERTED
preconditions
		(Agens, swc:hasAction, TheAgensAction) and
		(fabula:Assemble, rdfs:subClassOf, TheAgensAction)
	nextBlock
		(Patiens, swc:height, HeightPatiens)
	nextBlock
		(Patiens, swc:supportedBy, SupportingObject)
	nextBlock
		not(Controller, swc:controlledBy, Agens)
	nextBlock
		not(Target, rdf:type, SomeType)
	nextBlock
		(Patiens, swc:length, LengthPatiens)
	nextBlock
		(Agens, swcr:interactiveHeight, Patiens)
	nextBlock
		(Agens, swcr:interactiveLocation, Patiens)
	nextBlock
		(Agens, swc:controlledBy, aGENTID)
	nextBlock
		(Patiens, swc:width, WidthPatiens)
interEffects
	add
	del
effects
	add
		(Target, swc:weight, 0) and
		(Target, swc:height, HeightPatiens) and
		(Target, swc:length, LengthPatiens) and
		(Target, swc:width, WidthPatiens) and
		(Target, swc:supportedBy, SupportingObject) and
		(Target, rdf:type, swc:ConstructionHeap) and
		(Patiens, swc:partOfCorpuscularObject, Target) and
	del
		(Patiens, swc:supportedBy, SupportingObject) and 
. % End of createHeap


