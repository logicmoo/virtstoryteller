% goal database
% @author swartjes
% @date 27 september 2007

% -----------------------------------------------------------

% ----------------
% Wound the enemy
% ----------------
goal_schema([
	type(ps:'WoundEnemy'),
	arguments([	agens(Agens), patiens(Patiens) ]),
	preconditions([
		% Two characters, agens hates patiens
		condition(true, [
		    rule(Agens, owlr:typeOrSubType, fabula:'Character'),
	    	rule(Patiens, owlr:typeOrSubType, fabula:'Character'),
	    	fact(Agens, ps:hates, Patiens)
		]),
		% WoundEnemy is a unique goal (OOC, to prevent two characters having a plan to wound each other)
		condition(false, [
			fabula(_, rdf:type, fabula:'WoundEnemy')
		])
	]),
	success_conditions([
		% you have wounded the one you attack
		condition(true, [
			fact(Patiens,  ps:health, ps:wounded)   
		])
	])
]).

% ------------------------------
% Defend yourself when attacked
% ------------------------------
goal_schema([
	type(ps:'Defend'),
	arguments([	agens(Agens), patiens(Patiens) ]),
	urgency(0.7),
	preconditions([
		% There are two characters
		condition(true, [
		    rule(Agens, owlr:typeOrSubType, fabula:'Character'),
	    	rule(Patiens, owlr:typeOrSubType, fabula:'Character')
		]),
		% Patiens has said "en garde"
		condition(true, [
			fabula(Act, rdf:type, ps:'SayEnGarde'),
			fabula(Act, fabula:agens, Patiens)
		]),
		% I don't have the goal to wound
		condition(false, [
			fabula(Goal, rdf:type, ps:'WoundEnemy'),
			fabula(Goal, fabula:agens, Agens)
		])
	]),
	success_conditions([
		% you have wounded the one you attack
		condition(true, [
			fact(Patiens, ps:health, ps:wounded)   
		])
	]),
	failure_conditions([
		% You are wounded yourself
		condition(true, [
			fact(Agens, ps:health, ps:wounded)
		])
	])
]).
	
% ------------------------------------------
% Steal
% ------------------------------------------
% Goal for stealing something
% PRE:  agens is a thief
% SUC:  have the stolen object
% FAIL: someone else has the stolen object
goal_schema([
	type(ps:'Steal'),
	arguments([agens(Agens), patiens(Patiens), target(Target)]),
	preconditions([
		% Agens is a thief
	    condition(true, [
	    	fact(Agens, swc:hasRole, swc:'Thief')   % you must be a thief to want to steal something
	    ]),
	    % Target owns the object
	    condition(true, [
	    	fact(Target, ps:owns, Patiens)
	    ])
    ]),   
    success_conditions([
    	% Object held by agens
        condition(true, [
            fact(Patiens,  swc:heldBy, Agens)   % you're holding the stolen object
        ])
    ]),
    failure_conditions([
    	% Object held by other than yourself.
        condition(true, [    % someone else is holding the stolen object
            fact(Patiens, swc:heldBy, Other),
            rule(Agens, owlr:isNot, Other)
        ]) 
	])
]).
      
% ------------------------------------------------
% Refill the water supply
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
			
% --------------------
% Get drunk
% --------------------
goal_schema([
	type(ps:'GetDrunk'),
	arguments([	agens(Agens)]),
	preconditions([
		% Agens is thirsty
        condition(true, [
        	fact(Agens, swc:hasAttribute, ps:thirsty)
        ])
	]), 
	success_conditions([
		% Agens is drunk
		condition(true, [
			fact(Agens, swc:hasState, ps:'Drunk')
		])
	])
]). 
	
	
% DEBUG HELPER FUNCTIONS
validGoal(S, P) :-
    goal_schema(S), validate_schema(S, P).
    
validGoalFor(Agens, S, P) :-
    goal_schema(S), schema_agens(S, Agens), validate_schema(S, P).   
    
%rdf_global_id(ps:leChuck, Agens),schema_type(S, ps:'GetBottle'),validGoalFor(Agens,S,[]),goal_success_conditions(S,C),plan(Agens,C,Plan).