% goal database
% @author swartjes
% @date 27 september 2007

% -----------------------------------------------------------

 
% ------------------------------------------
% GetRumBottle
% ------------------------------------------
/*goal_schema([
	type(ps:'GetBottle'),
	arguments([	agens(Agens), patiens(Patiens)]),
	preconditions([
		condition(true, [
		    rule(Patiens, owlr:typeOrSubType, ps:'Bottle')
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
	*/

% ------------------------------------------
% GetTreasure
% ------------------------------------------
goal_schema([
	type(ps:'GetTreasure'),
	arguments([	agens(Agens), patiens(Patiens), contents(Contents)]),
	preconditions([
		condition(true, [
		    rule(Patiens, owlr:typeOrSubType, ps:'TreasureChest'),
		    rule(Contents, owlr:typeOrSubType, ps:'Gold'),
		    fact(Patiens, swc:contains, Contents)
		]),
		% Does not hold the treasure yet
		condition(false, [
		    fact(Patiens, swc:heldBy, Agens)
		])
	]), 
	success_conditions([
		condition(true, [
			fact(Patiens, swc:heldBy, Agens)
		    %fact(Patiens, swc:contains, Contents)
		    %fact(Agens, swc:supportedBy, ps:'oHarbor_1')		
		])
	])
]). 
	

goal_schema([
	type(ps:'Escape'),
	arguments([ agens(Agens), patiens(Patiens), target(Antagonist)]),
	preconditions([
		condition(true, [
			fact(Patiens, swc:heldBy, Agens),
			rule(Patiens, owlr:typeOrSubType, ps:'TreasureChest'),
			rule(Agens, owlr:isNot, Antagonist),
			rule(Antagonist, owlr:typeOrSubType, fabula:'Character')
		])
	]),
	success_conditions([
		condition(true, [
%			fact(Patiens, swc:heldBy, Agens),
%			fact(Agens, swc:supportedBy, ps:oHarbor_1),
			fact(Antagonist, ps:stuck, _)
			%fact(Antagonist, swc:supportedBy, ps:oBeach_1)
		])
		/*,
		condition(false, [
			fact(ps:'oShip_1', ps:'mooredAt', ps:'oSea_1')
		])*/
	])
]).

% ------------------------------------------
% GetGold
% ------------------------------------------
/*goal_schema([
	type(ps:'GetGold'),
	arguments([	agens(Agens), patiens(Patiens)]),
	preconditions([
		condition(true, [
		    rule(Patiens, owlr:typeOrSubType, ps:'Gold')
		]),
		% Does not hold the treasure yet
		condition(false, [
		    fact(Patiens, swc:heldBy, Agens)
		])
	]), 
	success_conditions([
		condition(true, [
			fact(Patiens, swc:heldBy, Agens)		
		])
	])
]). */
	


/*goal_schema([
	type(ps:'SailAway'),
	arguments([ agens(Agens),	patiens(Patiens)]),
	preconditions([
		condition(true, [
		    rule(Patiens, owlr:typeOrSubType, ps:'Ship')
		])
	]), 
	success_conditions([
		condition(true, [
			fact(Agens, swc:supportedBy, ps:oSea_1)
		])
	])
]).   	*/
	
	
	
% DEBUG HELPER FUNCTIONS
validGoal(S, P) :-
    goal_schema(S), validate_schema(S, P).
    
validGoalFor(Agens, S, P) :-
    goal_schema(S), schema_agens(S, Agens), validate_schema(S, P).   
    
%rdf_global_id(ps:leChuck, Agens),schema_type(S, ps:'GetBottle'),validGoalFor(Agens,S,[]),goal_success_conditions(S,C),plan(Agens,C,Plan).