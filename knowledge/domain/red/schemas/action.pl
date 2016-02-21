% action database for lollipop
% @author swartjes
% @date 21 jan 2008


% AGENS toddles off to TARGET
action_schema([
	type(red:'SkipTo'),
    arguments([agens(Agens), target(Target)]),
	duration(2),
	preconditions([
	    condition(true, [
	        fact(Agens, rdf:type, red:'LittleGirl'),
	        fact(Agens, swc:at, Location)
	    ]),
	    condition(true, [
	        rule(Path, owlr:typeOrSubType, swc:'Path')
	    ]),
	    condition(true, [
	        fact(Path, swc:from, Location),
	        fact(Path, swc:to, Target)
	    ])
	        
	]),
	effects([
		condition(true, [
		    fact(Agens,	swc:at, Target)
		 ]),
		 condition(false, [
            fact(Agens, swc:at, Location)
         ])
    ])
]).

% AGENS toddles off to TARGET
action_schema([
	type(red:'ShuffleTo'),
    arguments([agens(red:grandma), target(Target)]),
	duration(2),
	preconditions([
		condition(true, [
			fabula(_, rdf:type, red:'Poison')
		]),
	    condition(true, [
	        fact(red:grandma, swc:at, Location)
	    ]),
	    condition(true, [
	        rule(Path, owlr:typeOrSubType, swc:'Path')
	    ]),
	    condition(true, [
	        fact(Path, swc:from, Location),
	        fact(Path, swc:to, Target)
	    ])
	        
	]),
	effects([
		condition(true, [
		    fact(red:grandma,	swc:at, Target)
		 ]),
		 condition(false, [
            fact(red:grandma, swc:at, Location)
         ])
    ])
]).

% AGENS greets TARGET
action_schema([
	type(red:'Greet'),
	arguments([agens(Agens), target(Target)]),
	duration(1),
	preconditions([
		% Cannot greet yourself
		condition(true, [
			rule(Agens, owlr:isNot, Target),
	        rule(Location, owlr:typeOrSubType, swc:'Location')			
		]),
		% Must be at same location
		condition(true, [
            fact(Agens, swc:at, Location),
            fact(Target, swc:at, Location)
		])
	]),
	effects([
		condition(true, [
		 	fact(Agens, red:greeted, Target)
		])
	])
]).

% AGENS greets TARGET back
action_schema([
	type(red:'GreetBack'),
	arguments([agens(Agens), target(Target)]),
	duration(1),
	preconditions([
		% Cannot greet yourself back
		condition(true, [
			rule(Agens, owlr:isNot, Target),
	        rule(Location, owlr:typeOrSubType, swc:'Location')			
		]),
		% Must be at same location
		condition(true, [
            fact(Agens, swc:at, Location),
            fact(Target, swc:at, Location)
		])
	]),
	effects([
		condition(true, [
		 	fact(Agens, red:greeted, Target)
		])
	])
]).

% AGENS gives PATIENS to TARGET
action_schema([
	type(red:'Give'),
    arguments([agens(Agens), patiens(Patiens), target(Target)]),
	duration(1),
	preconditions([
	    condition(true, [
	        rule(Agens, owlr:isNot, Target),
	        rule(Agens, owlr:typeOrSubType, swc:'Character'),
	        rule(Target, owlr:typeOrSubType, swc:'Character'),
	        rule(Loc, owlr:typeOrSubType, swc:'Location')	        
	    ]),
	    condition(true, [
	        fact(Agens, swc:at, Loc),
	        fact(Target, swc:at, Loc)	        
	    ]),
	    condition(true, [
	        fact(Agens, swc:has, Patiens)
	    ])
	]),
	effects([
		condition(true, [
		    fact(Target, swc:has, Patiens)
		 ]),
		 condition(false, [
            fact(Agens, swc:has, Patiens)
         ])
    ])
]).

% AGENS takes PATIENS from TARGET
action_schema([
	type(red:'TakeFrom'),
    arguments([agens(Agens), patiens(Patiens), target(Target)]),
	duration(1),
	preconditions([
		% Two different characters and a location
	    condition(true, [
	        rule(Agens, owlr:isNot, Target),
	        rule(Agens, owlr:typeOrSubType, swc:'Character'),
	        rule(Target, owlr:typeOrSubType, swc:'Character'),
	        rule(Loc, owlr:typeOrSubType, swc:'Location')
	    ]),  
	    % I am mean
	    condition(true, [
	    	fact(Agens, swc:hasAttribute, red:mean)
	    ]),	    
	    % Target has the thing
	    condition(true, [
	        fact(Target, swc:has, Patiens)
	    ]),
	    % I do not own the thing (otherwise, use TakeBack)
	    condition(false, [
	    	fact(Agens, swc:owns, Patiens)
	    ]),
  		% At the same location
	    condition(true, [
	        fact(Agens, swc:at, Loc),
	        fact(Target, swc:at, Loc)	        
	    ]) 
	        
	]),
	effects([
		% I have the thing
		condition(true, [
		    fact(Agens, swc:has, Patiens)
		 ]),
		 % Target no longer has the thing
		 condition(false, [
            fact(Target, swc:has, Patiens)
         ])
    ])
]).

% AGENS eats TARGET
action_schema([
	type(red:'Eat'),
	arguments([agens(Agens), patiens(Patiens)]),
	duration(1),
	preconditions([
		% Theres a character and food
		condition(true, [
			rule(Agens, owlr:typeOrSubType, swc:'Character'),
			rule(Patiens, owlr:typeOrSubType, red:'Food')
		]),
		% Must have the food
		condition(true, [
            fact(Agens, swc:has, Patiens)
		])
	]),
	effects([
		% No longer hungry
		condition(false, [
		 	fact(Agens, swc:hasAttribute, red:hungry)
		]),
		% Food no longer at eater
		condition(false, [
			fact(Agens, swc:has, Patiens)
		])
	])
]).

% AGENS cries
action_schema([
	type(red:'Cry'),
	arguments([agens(Agens)]),
	duration(1),
	preconditions([
		% One character
		condition(true, [
			rule(Agens, owlr:typeOrSubType, swc:'Character')
		])
	]),
	effects([
	])
]).

% AGENS asks what to do
action_schema([
	type(red:'WhatToDo'),
	arguments([agens(Agens), target(Target)]),
	duration(1),
	preconditions([
		% Two characters
		condition(true, [	
			rule(Agens, owlr:typeOrSubType, swc:'Character'),
			rule(Target, owlr:typeOrSubType, swc:'Character')			
		]),
		% At same location
		condition(true, [
			fact(Agens, swc:at, Loc),
			fact(Target, swc:at, Loc)
		]),
		% You're not just going to say "what to do?". Have to have told what happened.
		condition(true, [
			fact(Agens, red:told_what_happened, Target)
		])
	]),
	effects([
	])
]).

% AGENS tells about the wolf taking something
% AGENS tells TARGET that INSTRUMENT took PATIENS (this is not a correct representation, but we don't support more complex sentences at this point)
action_schema([
	type(red:'TellSomeoneTookSomething'),
	arguments([agens(Agens), patiens(Patiens), target(Target), instrument(MeanOne)]),
	duration(1),
	preconditions([
		% Two characters
		condition(true, [	
			rule(Agens, owlr:typeOrSubType, swc:'Character'),
			rule(Target, owlr:typeOrSubType, swc:'Character')			
		]),
		% At same location
		condition(true, [
			fact(Agens, swc:at, Loc),
			fact(Target, swc:at, Loc)
		]),
		% The wolf really DID take something
		condition(true, [
			fabula(Act, rdf:type, red:'TakeFrom'),
			fabula(Act, fabula:agens, MeanOne),
			fabula(Act, fabula:patiens, Patiens),
			fabula(Act, fabula:target, Agens)
		])
	]),
	effects([
		condition(true, [
			fact(Agens, red:told_what_happened, Target)
		])
	])
]).

% AGENS bakes a cake
action_schema([
	type(red:'BakeCake'),
	arguments([agens(red:grandma), patiens(Patiens)]),
	duration(3),
	preconditions([
		% Unbaked
		condition(true, [
			fact(Patiens, swc:hasAttribute, red:unbaked)
		]),
		% Grandma is at home
		condition(true, [
			fact(red:grandma, swc:at, red:grandmas_house)
		]),	
		% Patiens is a cake
		condition(true, [
			rule(Patiens, owlr:typeOrSubType, red:'Cake')
		]),
		% But not located anywhere yet
		condition(false, [
			fact(Patiens, swc:at, _)
		])

	]),
	effects([
		% Agens has (baked) the cake
		condition(true, [
			fact(red:grandma, swc:has, Patiens)
		]),
		% No longer an unbaked cake
		condition(false, [
			fact(Patiens, swc:hasAttribute, red:unbaked)
		])
	])
]).

% AGENS poisons food
action_schema([
	type(red:'PoisonFood'),
	arguments([agens(Agens), patiens(Patiens)]),
	duration(1),
	preconditions([
		% Patiens is food
		condition(true, [
			rule(Patiens, owlr:typeOrSubType, red:'Food')
		]),
		% I have the food
		condition(true, [
			fact(Agens, swc:has, Patiens)
		]),
		% I am at grandma's house
		condition(true, [
			fact(Agens, swc:at, red:grandmas_house)
		])
	]),
	effects([
		% Food is poisoned
		condition(true, [
			fact(Patiens, swc:hasAttribute, red:poisoned)
		])
	])
]).

% Speech act for communicating "Poison" goal.
% AGENS tells TARGET that he/she wants to poison PATIENS
action_schema([
	type(red:'TellAboutPoisonPlan'),
	arguments([agens(Agens), patiens(Patiens), target(Target)]),
	duration(1),
	preconditions([
		% I have a poison goal.
		condition(true, [
			fabula(G, rdf:type, red:'Poison'),
			fabula(G, fabula:agens, Agens),
			fabula(G, fabula:patiens, Patiens),
			fabula(G, fabula:target, Target)
		]),
		% We are co-located.
		condition(true, [
			fact(Agens, swc:at, Loc),
			fact(Target, swc:at, Loc)
		])
	]),
	effects([
	])
]).
