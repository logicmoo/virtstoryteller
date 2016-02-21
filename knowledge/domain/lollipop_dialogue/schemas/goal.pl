% goal database
% @author swartjes
% @date 27 september 2007

% -----------------------------------------------------------

	
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

% ------------------------------------------
% SellIceGeneral
% ------------------------------------------
% Goal for having ice
goal_schema([
	type('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#SellIceGeneral'),
	arguments([agens(Agens), patiens(Patiens)]),
	urgency(0.1),
	preconditions([
		% IF I am an ice vendor, and I have ice
	    condition(true, [
	        fact(Agens, rdf:type, lolli:'IceVendor'),
	        fact(Patiens, rdf:type, lolli:'IceCream'),
	        fact(Agens, lolli:has, Patiens)
	    ])
	]), 
    success_conditions([
    	condition(true, [
    		fact(lolli:easter, lolli:sameday, lolli:whitsunday)
    	])
    ])
]).

% ------------------------------------------
% GiveIce
% ------------------------------------------
% Goal for giving ice
goal_schema([
	type('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#GiveIce'),
	arguments([agens(Agens), patiens(Patiens), target(Target)]),
	preconditions([
		% IF I want to sell ice
		condition(true, [
			fabula(Goal, rdf:type, lolli:'SellIceGeneral'),
			fabula(Goal, fabula:agens, Agens),
			fabula(Goal, fabula:patiens, Patiens)
		]),
	    % And someone asked for ice
	    condition(true, [
	    	fabula(Act, rdf:type, lolli:'AskFor'),
	    	fabula(Act, fabula:agens, Target),
	    	fabula(Act, fabula:target, Agens),
	    	fabula(Act, fabula:patiens, Patiens)
	    ]),
	    % And I have been paid
	    condition(true, [
	    	fact(Target, lolli:paid, Agens)
	    ])
	]), 
    success_conditions([
        % I want them to have it
        condition(true, [
        	fact(Target, lolli:has, Patiens)
        ])
    ])
]).

% ------------------------------------------
% SellIce
% ------------------------------------------
% Goal for having money
goal_schema([
	type('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#GetMoney'),
	arguments([agens(Agens), patiens(Patiens), target(Target)]),
	preconditions([
		% IF I want to sell ice
		condition(true, [
			fabula(Goal, rdf:type, lolli:'SellIceGeneral'),
			fabula(Goal, fabula:agens, Agens),
			fabula(Goal, fabula:patiens, Patiens)
		]),
	    % And someone asked for ice
	    condition(true, [
	    	fabula(Act, rdf:type, lolli:'AskFor'),
	    	fabula(Act, fabula:agens, Target),
	    	fabula(Act, fabula:target, Agens),
	    	fabula(Act, fabula:patiens, Patiens)
	    ]),
	    % And that someone has money (implies they haven't given it yet)
	    condition(false, [
	    	fact(Target, lolli:paid, Agens)
	    ])
	]), 
    success_conditions([
        % I want to get paid
        condition(true, [
        	fact(Target, lolli:paid, Agens)
        ])
    ])
]).

% ------------------------------------------
% Greet
% ------------------------------------------
% Goal for greeting
goal_schema([
	type('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#GreetGoal'),
	arguments([agens(Agens), patiens(Patiens)]),
	urgency(0.8),	
	preconditions([
		% Not greet yourself
		condition(true, [
			rule(Agens, owlr:isNot, Patiens)
		]),
		% Not greeted yet
	    condition(false, [
	        fact(Agens, lolli:greeted, Patiens)
	    ]),
	    condition(true, [
	    	fact(Agens, lolli:at, Loc),
	    	fact(Patiens, lolli:at, Loc)
	    ])
	]), 
    success_conditions([
        condition(true, [
            fact(Agens, lolli:greeted, Patiens)
        ])
    ])
]).

% ------------------------------------------
% GreetBack
% ------------------------------------------
% Goal for greeting back
goal_schema([
	type('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#GreetBack'),
	arguments([agens(Agens), patiens(Patiens)]),
	urgency(0.8),	
	preconditions([
	    condition(true, [
	        fact(Patiens, lolli:greeted, Agens)
	    ]),
	    condition(true, [
	    	fact(Agens, lolli:at, Loc),
	    	fact(Patiens, lolli:at, Loc)
	    ]),
	    condition(false, [
	        fact(Agens, lolli:greeted, Patiens)
	    ])
	]), 
    success_conditions([
        condition(true, [
            fact(Agens, lolli:greeted, Patiens)
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