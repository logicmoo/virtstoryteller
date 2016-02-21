% goal database
% @author swartjes
% @date 27 september 2007

% -----------------------------------------------------------

	
% ------------------------------------------
% Bring cake
% ------------------------------------------
% Goal for bringing cake
% Any character that has a cake, wants to bring the cake to grandma if it isn't there already.
goal_schema([
	type(red:'BringCake'),
	arguments([agens(Agens), patiens(Patiens)]),
	preconditions([
		condition(true, [
			rule(Agens, owlr:typeOrSubType, red:'LittleGirl'),
	        rule(Patiens, owlr:typeOrSubType, red:'Cake')
		]),
	    condition(true, [
	        fact(Agens, swc:has, Patiens)
	    ]),
	    condition(false, [
	        fact(red:grandma, swc:has, Patiens)
	    ])
	]), 
    success_conditions([
        condition(true, [
            fact(red:grandma, swc:has, Patiens)
        ])
    ])
]).

% ------------------------------------------
% Eat
% ------------------------------------------
% Goal for eating
% Any character that is hungry, wants to eat.
goal_schema([
	type(red:'EatSomething'),
	arguments([agens(Agens)]), %, patiens(Patiens)]),
	preconditions([
		condition(true, [
			rule(Agens, owlr:typeOrSubType, swc:'Character')
%	        rule(Patiens, owlr:typeOrSubType, red:'Food')
		]),
		condition(true, [
			fact(Agens, swc:hasAttribute, red:hungry)
		])
	]), 
    success_conditions([
      %  condition(true, [
        %    fabula(Act, rdf:type, red:'Eat'),
        %    fabula(Act, fabula:agens, Agens),
        %    fabula(Act, fabula:patiens, Patiens)
       % ])
    	condition(false, [
    		fact(Agens, swc:hasAttribute, red:hungry)
    	])
       
    ])
]).

% ------------------------------------------
% Seek support
% ------------------------------------------
% Goal for seeking support when crying
goal_schema([
	type(red:'SeekSupport'),
	arguments([agens(Agens), target(red:grandma)]),
	urgency(0.6),
	preconditions([
		% We are different characters
		condition(true, [
			rule(Agens, owlr:isNot, red:grandma),
			rule(Agens, owlr:typeOrSubType, swc:'Character')
		]),
		% I have just been crying
		condition(true, [
			fabula(Act, rdf:type, red:'Cry'),
			fabula(Act, fabula:agens, Agens)
		])
	]), 
    success_conditions([
		% I have asked: what should I do?
        condition(true, [
            fabula(Act2, rdf:type, red:'WhatToDo'),
            fabula(Act2, fabula:agens, Agens),
            fabula(Act2, fabula:target, red:grandma)
        ])       
    ])
]).

% ------------------------------------------
% Seek support
% ------------------------------------------
% Goal for poisoning a badass
% AGENS wants to poison PATIENS, telling TARGET
goal_schema([
	type(red:'Poison'),
	arguments([agens(Agens), patiens(Patiens), target(Target)]),
	urgency(0.6),
	preconditions([
		% We are different characters
		condition(true, [
			rule(Agens, owlr:isNot, Patiens),
			rule(Agens, owlr:isNot, Target),
			rule(Patiens, owlr:isNot, Target),
			rule(Agens, owlr:typeOrSubType, swc:'Character'),
			rule(Patiens, owlr:typeOrSubType, swc:'Character'),
			rule(Target, owlr:typeOrSubType, swc:'Character')
		]),
		% Someone just asked me for advice
		condition(true, [
			fabula(Act, rdf:type, red:'WhatToDo'),
			fabula(Act, fabula:agens, Target),
			fabula(Act, fabula:target, Agens)
		])
	]), 
    success_conditions([
		% Patiens is poisoned
        condition(true, [
            fact(Patiens, swc:hasAttribute, red:dead)
        ])       
    ])
]).


% -----------------------------------------------------------
    	
% DEBUG HELPER FUNCTIONS
validGoal(S, P) :-
    goal_schema(S), validate_schema(S, P).
    
validGoalFor(Agens, S, P) :-
    goal_schema(S), schema_agens(S, Agens), validate_schema(S, P).    
    
%validGoalFor(Agens,S,[]),goal_success_conditions(S,C),plan(Agens,C,Plan).    

% plan('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#linda', [condition(true, [fact('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#linda', 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#has', 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#vanilla_ice_1')]), condition(true, [fact('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#linda', 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#at', 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#park_1')])], Plan).