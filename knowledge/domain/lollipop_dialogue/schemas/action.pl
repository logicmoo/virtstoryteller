% action database for lollipop
% @author swartjes
% @date 21 jan 2008


% AGENS toddles off to TARGET
action_schema([
	type(lolli:'ToddleOffTo'),
    arguments([agens(Agens), target(Target), location(Location)]),
	duration(1),
	preconditions([
	    condition(true, [
	        fact(Agens, rdf:type, lolli:'Kid'),
	        fact(Agens, lolli:at, Location),
	        fact(Location, lolli:adjacent, Target),
	        rule(Location, owlr:isNot, Target)	    
		])
	]),
	effects([
		condition(true, [
		    fact(Agens,	lolli:at, Target)
		 ]),
		 condition(false, [
            fact(Agens, lolli:at, Location)
         ])
    ])
]).

% AGENS greets TARGET
action_schema([
	type(lolli:'Greet'),
	arguments([agens(Agens), target(Target)]),
	duration(1),
	preconditions([
		% Cannot greet yourself
		condition(true, [
			rule(Agens, owlr:isNot, Target)
		]),
		% Must be at same location
		condition(true, [
            fact(Agens, lolli:at, Location),
            fact(Target, lolli:at, Location)
		])
	]),
	effects([
		condition(true, [
		 	fact(Agens, lolli:greeted, Target)
		])
	])
]).

% AGENS asks TARGET for PATIENS
action_schema([
	type(lolli:'AskFor'),
	arguments([agens(Agens),target(Target),patiens(Patiens)]),
	duration(1),
	preconditions([
		condition(true, [
			rule(Agens, owlr:isNot, Target)
		]),
        condition(true, [
            fact(Target, lolli:has, Patiens)
		]),
/*        condition(true, [
          	fact(Agens, lolli:greeted, Target),
          	fact(Target, lolli:greeted, Agens)
        ]),	*/
		condition(true, [
            fact(Agens, lolli:at, Location),
            fact(Target, lolli:at, Location)
        ]) 		
	]),
	effects([
		
	])
]).

% AGENS sells PATIENS to TARGET
action_schema([
	type(lolli:'Sell'),
	arguments([agens(Agens),target(Target),patiens(Patiens)]),
	duration(1),
	preconditions([
		condition(true, [
			%Agens heeft Patiens
            fact(Agens, lolli:has, Patiens)		
		]),
		condition(true, [
			rule(Agens, owlr:isNot, Target)
		]),			
		condition(true, [
			%beiden zijn op dezelfde positie
			fact(Agens, lolli:at, Location),
            fact(Target, lolli:at, Location)
		]),
		% Je bent betaald
		condition(true, [
			fact(Target, lolli:paid, Agens)
		])
	]),
	effects([
		condition(true, [
			fact(Target, lolli:has, Patiens)
		]),
		condition(false, [
			fact(Agens, lolli:has, Patiens)
		])
	])
]).

% AGENS pays PATIENS with INSTRUMENT
action_schema([
	type(lolli:'Pay'),
	arguments([agens(Agens),patiens(Patiens), instrument(Instrument)]),
	duration(1),
	preconditions([
		condition(true, [
			%Agens heeft geld
            fact(Agens, lolli:has, Instrument),
            rule(Instrument, owlr:typeOrSubType, lolli:'Money')	
		]),
		% Cannot pay yourself
		condition(true, [
			rule(Agens, owlr:isNot, Patiens)
		]),			
		condition(true, [
			%beiden zijn op dezelfde positie
			fact(Agens, lolli:at, Location),
            fact(Patiens, lolli:at, Location)
		])	
	]),
	effects([
		condition(true, [
			fact(Patiens, lolli:has, Instrument),
			fact(Agens, lolli:paid, Patiens)
		]),
		condition(false, [
			fact(Agens, lolli:has, Instrument)
		])
	])
]).

% AGENS gives PATIENS to TARGET
/*
action_schema([
	type(lolli:'GiveTo'),
	arguments([agens(Agens),target(Target),patiens(Patiens)]),
	duration(1),
	preconditions([
		condition(true, [
			%Agens heeft Patiens
            fact(Agens, lolli:has, Patiens)		
		]),
		condition(true, [
			rule(Agens, owlr:isNot, Target)
		]),			
		condition(true, [
			%beiden zijn op dezelfde positie
			fact(Agens, lolli:at, Location),
            fact(Target, lolli:at, Location)
		])
	]),
	effects([
		condition(true, [
			fact(Target, lolli:has, Patiens)
		]),
		condition(false, [
			fact(Agens, lolli:has, Patiens)
		])
	])
]).*/

