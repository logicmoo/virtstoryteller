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
	type(ps:'WoundEnemy'),
	arguments([	agens(Agens), patiens(Patiens) ]),
	preconditions([
		condition(true, [
		    rule(Agens, owlr:typeOrSubType, fabula:'Character'),
	    	rule(Patiens, owlr:typeOrSubType, fabula:'Character'),
	    	fact(Agens, ps:hates, Patiens)
		])
	]),
	success_conditions([
		% you have wounded the one you attack
		condition(true, [
			fact(Patiens,  ps:health, ps:wounded)   
		])
	])
]).
		
% DEBUG HELPER FUNCTIONS
validGoal(S, P) :-
    goal_schema(S), validate_schema(S, P).
    
validGoalFor(Agens, S, P) :-
    goal_schema(S), schema_agens(S, Agens), validate_schema(S, P).   
    
%schema_type(S, ps:'WoundEnemy'),validGoalFor(Agens,S,[]),goal_success_conditions(S,C),plan(Agens,C,Plan).