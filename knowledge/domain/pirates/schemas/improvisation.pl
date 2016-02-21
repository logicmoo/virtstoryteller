schema([    
    head([
        type('http://www.owl-ontologies.com/FabulaKnowledge.owl#CreateRumBottle')
        ]),
    class(improvisation), 
    posPreconditions([
        (Crate, rdf:type, swc:'Container')
    ]),
    
    posEffects([
        ('http://www.owl-ontologies.com/StoryWorldSettings/Pirates#oRumBottle_1', 'http://www.owl-ontologies.com/StoryWorldCore.owl#containedBy', Crate)
        %('http://www.owl-ontologies.com/StoryWorldSettings/Pirates#oRumBottle_1', 'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Pirates#oHold_1')
    ])]).   
    
schema([    
    head([
        type('http://www.owl-ontologies.com/FabulaKnowledge.owl#CreateBottleCrate')
        ]),
    class(improvisation), 
    negPreconditions([
    	(ps:oCrate_1, rdf:type, swc:'Container')
    ]),
    
    posEffects([   	
        (ps:oCrate_1, swc:supportedBy, ps:oHold_1),
        (ps:oCrate_1, rdf:type, swc:'Container')
        %('http://www.owl-ontologies.com/StoryWorldSettings/Pirates#oRumBottle_1', 'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Pirates#oHold_1')
    ])]).       