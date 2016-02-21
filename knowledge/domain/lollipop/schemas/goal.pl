% goal database
% @author swartjes
% @date 27 september 2007

% -----------------------------------------------------------


% ------------------------------------------
% BoughtIce
% ------------------------------------------
% Goal for having bought ice (experiment)
goal_schema([
	type('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#BoughtIce'),
	arguments([agens(Agens), patiens(Patiens)]),
	preconditions([
	    condition(true, [
	        fabula(Agens, rdf:type, lolli:'Kid'),
	        fact(Patiens, rdf:type, lolli:'IceCream')
	    ])
	]), 
    success_conditions([
        condition(true, [
            fabula(Act, rdf:type, lolli:'Buy'),
            fabula(Act, fabula:agens, Agens),
            fabula(Act, fabula:patiens, Patiens)
        ])
    ])
]).

	
% ------------------------------------------
% HaveIce
% ------------------------------------------
% Goal for having ice
goal_schema([
	type('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#HaveIce'),
	arguments([agens(Agens), patiens(Patiens)]),
	preconditions([
	    condition(true, [
	        fact(Agens, rdf:type, lolli:'Kid'),
	        fact(Patiens, rdf:type, lolli:'IceCream')
	    ])
	]), 
    success_conditions([
        condition(true, [
            fact(Agens, lolli:has, Patiens)
        ])
    ])
]).

    	
% DEBUG HELPER FUNCTIONS
validGoal(S, P) :-
    goal_schema(S), validate_schema(S, P).
    
validGoalFor(Agens, S, P) :-
    goal_schema(S), schema_agens(S, Agens), validate_schema(S, P).    
    
%validGoalFor(Agens,S,[]),goal_success_conditions(S,C),plan(Agens,C,Plan).    

% plan('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#linda', [condition(true, [fact('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#linda', 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#has', 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#vanilla_ice_1')]), condition(true, [fact('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#linda', 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#at', 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#park_1')])], Plan).