% action database for lollipop
% @author swartjes
% @date 21 jan 2008

% AGENS buys PATIENS from TARGET with INSTRUMENT
action_schema([
	type(lolli:'Buy'),
    arguments([agens(Agens), patiens(Patiens), target(Target), instrument(Instrument), location(Location)
		]),
	duration(1),
	preconditions([
        condition(true, [
            fact(Instrument, rdf:type, lolli:'Money'),
            fact(Agens, lolli:has, Instrument),
            fact(Agens, lolli:at, Location),
            fact(Target, lolli:at, Location),
            fact(Target, lolli:has, Patiens)
          	%fact(Target, lolli:greeted, Agens), 
		])
	]),
	effects([
		condition(true, [
		    fact(Agens,	lolli:has,	Patiens)
		]),
        condition(false, [
            fact(Agens, lolli:has, Instrument)
        ]),
        condition(false, [
            fact(Target, lolli:has, Patiens)
        ])
    ])
]).

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

action_schema([
	type(lolli:'Dance'),
	arguments([agens(_)]),
	duration(1),
	preconditions([]),
	effects([])
]).
