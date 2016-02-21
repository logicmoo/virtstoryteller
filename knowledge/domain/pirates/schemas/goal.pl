% goal database
% @author swartjes
% @date 27 september 2007

% -----------------------------------------------------------


	
% ------------------------------------------
% Steal
% ------------------------------------------
% Goal for stealing something
% PRE:  agens is a thief
% SUC:  have the stolen object
% FAIL: someone else has the stolen object
schema([
	head([
		type('http://www.owl-ontologies.com/PiratesSetting.owl#Steal'),
		agens(Agens), patiens(Patiens)
		]), 
	class(goal),
	posPreconditions([(Agens, swc:hasRole, swc:'Thief')]),   % you must be a thief to want to steal something
	steps([
			[
    			posSuccessConditions([(Patiens,  swc:heldBy, Agens)]),  % you're holding the stolen object
    			posFailureConditions([(Patiens, swc:heldBy, Other),
        			                  (Agens, owlr:isNot, Other)])  % someone else is holding the stolen object
    		]
    	])
	]).
    
% ------------------------------------------
% GetTreasure    
% ------------------------------------------
% Goal for getting the treasure
% PRE:  there is a treasurechest
% SUC:  agens has the treasurechest
% FAIL: someone else has the treasurechest
schema([
	head([
		type('http://www.owl-ontologies.com/StoryWorldSettings/Pirates#GetTreasure'),
		agens(Agens), patiens(Patiens), patiens(Shovel), other(Other)
		]),
	class(goal),
	posPreconditions([(Patiens, owlr:typeOrSubType, ps:'TreasureChest'), (Shovel, owlr:typeOrSubType, ps:'Shovel')]), 
    steps([
			[
				posSuccessConditions([(Shovel,  swc:heldBy, Agens)])
		 	],
			[
				posSuccessConditions([(Patiens,  swc:heldBy, Agens)]),
		 		posFailureConditions([(Patiens, swc:heldBy, Other), (Agens, owlr:isNot, Other)])
			]
    	])
    
    ]).  
    
% ------------------------------------------
% BecomeDrunk
% ------------------------------------------
% Goal for "becoming drunk" (HOW TO DO THIS??)
schema([
	head([
		type('http://www.owl-ontologies.com/StoryWorldSettings/Pirates#BecomeDrunk'),
		agens(Agens)
		]),
	class(goal),
	negPreconditions([(Agens, swc:hasProperty, ps:'Drunk')]), % You are not drunk yet
	steps([
			[	
				posSuccessConditions([(Agens,  swc:hasProperty, ps:'Drunk')]) % Success if now drunk.
			]
		])
	
    ]).   
 
% ------------------------------------------
% GetRumBottle
% ------------------------------------------
schema([
	head([
		type('http://www.owl-ontologies.com/StoryWorldSettings/Pirates#GetBottle'),
		agens(Agens), patiens(Patiens)
		]),
	class(goal),	
	posPreconditions([
	    (Patiens, owlr:typeOrSubType, ps:'Bottle')
	]), 
	steps([
			[
				posSuccessConditions([(Patiens, swc:heldBy, Agens)])
    		]
    	]) 
    ]).   
    
schema([
	head([
		type('http://www.owl-ontologies.com/StoryWorldSettings/Pirates#GotoHold'),
		agens(Agens), patiens('http://www.owl-ontologies.com/StoryWorldSettings/Pirates#oHold_1')
		]),
	class(goal),	
	posPreconditions([
	]), % You are not drunk yet
	steps([
			[
				posSuccessConditions([(Agens, swc:supportedBy, 'http://www.owl-ontologies.com/StoryWorldSettings/Pirates#oHold_1')]) % Success if now drunk.
    		]
    	])
    ]).       
    
 
    
% ------------------------------------------
% GoTo
% ------------------------------------------
% Goal for going to some place
% PRE: at some location from which the target is reachable
% SUC: at target
schema([
	head([
		type('http://www.owl-ontologies.com/PiratesSetting.owl#GoTo'),
		agens(Agens), target(Target), location(CurLoc)
		]), 
	class(goal),
	posPreconditions([(Agens, swcr:atLocation, CurLoc),
					   (Target, swcr:reachableFrom, CurLoc)]),
	steps([
			[   
    			posSuccessConditions([(Agens,  swc:supportedBy, Target)])
    		]
    	])
	]).        
	
% DEBUG HELPER FUNCTIONS
validGoal(S, PP, NP) :-
    schema(S), getFromSchema(S, head(H)), getFromSchema(S, class(goal)), validateOperator(H, PP, NP).
    
validGoalFor(Agens, S, PP, NP) :-
    schema(S), getFromSchema(S, head(H)), getFromSchema(S, class(goal)), getFromHead(H, agens(Agens)), validateOperator(H, PP, NP).    