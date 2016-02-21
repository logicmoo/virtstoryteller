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
goal_schema([
	type(ps:'Steal'),
	arguments([agens(Agens), patiens(Patiens)]),
	preconditions([
	    condition(true, [
	    	fact(Agens, swc:hasRole, swc:'Thief')   % you must be a thief to want to steal something
	    ])
    ]),   
    success_conditions([
        condition(true, [
            (Patiens,  swc:heldBy, Agens)   % you're holding the stolen object
        ])
    ]),
    failure_conditions([
        condition(true, [    % someone else is holding the stolen object
            fact(Patiens, swc:heldBy, Other),
            rule(Agens, owlr:isNot, Other)
        ]) 
	])
]).
    
% ------------------------------------------
% GetTreasure    
% ------------------------------------------
% Goal for getting the treasure
% PRE:  there is a treasurechest
% SUC:  agens has the treasurechest
% FAIL: someone else has the treasurechest
goal_schema([
	type(ps:'GetTreasure'),
	arguments([	agens(Agens), patiens(Patiens), patiens(Shovel), other(Other) ]),
	urgency(0.4),
	preconditions([
		% There is a treasure chest and a shovel
		condition(true, [
			rule(Patiens, owlr:typeOrSubType, ps:'TreasureChest'),
			rule(Shovel, owlr:typeOrSubType, ps:'Shovel')
		]),
		% Haven't already had a goal to get the treasure.
		% Careful, this currently (28 jul 08) means: "There is no GetTreasure instance OR I am not the agens of anything"
		condition(false, [
			fabula(G, rdf:type, ps:'GetTreasure'),
			fabula(G, fabula:agens, Agens)
		])
	]),
	success_conditions([
		% Sucess when I hold the treasure chest
		condition(true, [
			fact(Patiens,  swc:heldBy, Agens)
		])
	]),
	failure_conditions([
		% Fail when someone else holds the treasure chest
		condition(true, [
			fact(Patiens,  swc:heldBy, Other),
			rule(Agens, owlr:isNot, Other)
		])   
	])
]).  
    

    
% ------------------------------------------------
% Test goal for communicating goal to get some rum
% ------------------------------------------------

goal_schema([
	type(ps:'AnnounceRum'),
	arguments([ agens(Agens) ]),
	urgency(0.9),
	preconditions([
		% There is a GetBottle goal that I have (had)
		condition(true, [
			fabula(S, rdf:type, ps:'GetBottle'),
			fabula(S, fabula:character, Agens)
		]),
		% And that hasn't motivated me to announce it yet
		% TODO: when motivation is implemented, use fabula:motivates
		% for now, check whether "AnnounceRum" has already been a goal.
		% Careful, this currently (28 jul 08) means: "There is no AnnounceRum instance, OR I am not the character of anything"
		condition(false, [
			fabula(T, rdf:type, ps:'AnnounceRum'),
			fabula(T, fabula:character, Agens)
		])		
	]),
	success_conditions([
		% Success when I have said "let's get some rum"
		% TODO: also too broad; success also depends on whether this was motivated by this particular AnnounceRum?
		condition(true, [
			fabula(T, rdf:type, ps:'SayLetsGetSomeRum'),
			fabula(T, fabula:agens, Agens)			
		])
	])
]).

% ------------------------------------------------
% Test goal for refilling the water supply
% ------------------------------------------------
goal_schema([
	type(ps:'RefillWaterSupply'),
	arguments([ agens(Agens), patiens(WaterSupply) ]),
	preconditions([
		% I am the captain of a ship
		condition(true, [
			fact(Agens, rdf:type, ps:'Captain'),
			fact(Agens, ps:owns, Ship),
			rule(Ship, owlr:typeOrSubType, ps:'Ship')			
		]),
		% I am on the ship
		condition(true, [
			rule(Agens, swcr:locatedAtTransitive, Ship)
		]),
		% And the ship has an empty water supply
		condition(true, [
			rule(Ship, owlr:typeOrSubType, ps:'Ship'),
			fact(Ship, ps:hasWaterSupply, WaterSupply),
			fact(WaterSupply, ps:hasFullEmptyProperty, ps:empty)
		])
	]),
	success_conditions([
		% The ship has a full water supply again
		condition(true, [
			fact(WaterSupply, ps:hasFullEmptyProperty, ps:full)
		])
	])
]).
			
   
 
% ------------------------------------------
% GetRumBottle
% ------------------------------------------
goal_schema([
	type(ps:'GetBottle'),
	arguments([	agens(Agens), patiens(Patiens)]),
	preconditions([
		% There is a bottle
		condition(true, [
		    rule(Patiens, owlr:typeOrSubType, ps:'Bottle') % disabled!
		]),
		% Does not hold a bottle yet
		condition(false, [
		    fact(Patiens, swc:heldBy, Agens)
		])
	]), 
	success_conditions([
		condition(true, [
			fact(Patiens, swc:heldBy, Agens)
		])
	])
]).   	
	
	
% DEBUG HELPER FUNCTIONS
validGoal(S, P) :-
    goal_schema(S), validate_schema(S, P).
    
validGoalFor(Agens, S, P) :-
    goal_schema(S), schema_agens(S, Agens), validate_schema(S, P).   
    
%rdf_global_id(ps:leChuck, Agens),schema_type(S, ps:'GetBottle'),validGoalFor(Agens,S,[]),goal_success_conditions(S,C),plan(Agens,C,Plan).