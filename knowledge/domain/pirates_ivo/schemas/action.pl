% action database for pirates
% @author swartjes
% @date 26 nov 2007

action_schema([
	type(ps:'SailToLand'),
	arguments([ agens(Agens), target(Target), instrument(Ship) ]),
	duration(2),
	preconditions([
		% Instrument is a ship
		condition(true, [
			rule(Ship, owlr:typeOrSubType, ps:'Ship')
		]),
		% Of which I am the captain
		condition(true, [
			rule(Agens, owlr:typeOrSubType, ps:'Captain'),
			fact(Agens, ps:owns, Ship)
		]),
		% And which is not moored somewhere
		condition(false, [
			fact(Ship, ps:mooredAt, _)
		]),		
		% Target is a land one can sail to
		condition(true, [
			rule(Target, owlr:typeOrSubType, ps:'LandArea')
		])
	]),
	effects([
		% Ship is at target
		condition(true, [
			fact(Ship, ps:mooredAt, Target)
		])
	])
]).

action_schema([
	type(ps:'GetOffBoat'),
	arguments([ agens(Agens), location(Deck), target(Land) ]),
	duration(1),
	preconditions([
		% I am on the deck of the boat
		condition(true, [
			rule(Deck, owlr:typeOrSubType, ps:'Deck'),
			fact(Agens, swc:supportedBy, Deck),
			fact(Ship, swc:hasRegion, Deck)
		]),	
		% The boat is moored somewhere
		condition(true, [
			fact(Ship, ps:mooredAt, Land)
		])	
	]),
	effects([
		% I am now on the land
		condition(true, [
			fact(Agens, swc:supportedBy, Land)
		]),
		% And no longer on the deck
		condition(false, [
			fact(Agens, swc:supportedBy, Deck)
		])
	])
]).

action_schema([
	type(ps:'FillBoatWithWater'),
	arguments([ agens(Agens), patiens(Boat), instrument(Water) ]),
	duration(1),
	preconditions([
		% There is water
		condition(true, [
			fact(Water, rdf:type, ps:'Water')
		]),			
		% The boat is moored at this land, and has an empty water supply
		condition(true, [
			fact(Boat, ps:mooredAt, Land),
			fact(Boat, ps:hasWaterSupply, Supply),
			fact(Supply, ps:hasFullEmptyProperty, ps:empty)
		]),	
		% I am near water
		condition(true, [
			fact(Agens, swc:supportedBy, Land),
			fact(Water, swc:supportedBy, Land)
		])	
	]),
	effects([
		% Boat's water supply is replenished
		condition(true, [
			fact(Supply, ps:hasFullEmptyProperty, ps:full)
		])
	])
]).

action_schema([
	type(ps:'SayLetsGetSomeRum'),
	arguments([	agens(Ag) ]),
	duration(1),
	preconditions([
		% I must be a character
		condition(true, [
			rule(Ag, owlr:typeOrSubType, fabula:'Character')
		])
	]),
	effects([])
]).

action_schema([
	type('http://www.owl-ontologies.com/FabulaKnowledge.owl#DrinkRum'),
	arguments([agens(Ag), patiens(Pa)]),
	duration(1),
	preconditions([
		% I must know how to drink rum
		condition(true, [
		    rule(Ag, swcr:knowsAction, fabula:'DrinkRum')
	    ]),
	    % There must be a rum bottle
		condition(true, [
		    rule(Pa, owlr:typeOrSubType, ps:'RumBottle')
		]),
		% I must be holding the rum bottle
		condition(true, [
		    fact(Pa, swc:heldBy, Ag)
		]),
		% The rum bottle must contain rum
		condition(true, [
		    fact(Pa, swc:contains, ps:'Rum')	   
		])
	]),
    effects([
    	% I am now drunk
	    condition(true, [
			fact(Ag, swc:hasState, ps:'Drunk')
		]),
		% The bottle contains no rum anymore
		condition(false, [
    		fact(Pa, swc:contains, ps:'Rum')
		])
	])
]).

/*action_schema([
	type(fabula:'OpenHatch'),
    arguments([    agens(Agens), patiens(ps:oHatch_1), road(Road) ]),
    duration(1),
    preconditions([
    	
    	 % Agens can do the action
		condition(true, [
			rule(Agens,	swcr:knowsAction, fabula:'OpenDoor')
		]),
		
		% Instrument is located "at" a door that is openable, but closed.
		condition(true, [
			fact(Agens, swc:supportedBy, CurrLoc),
			fact(Road, swc:connectedToGeographicArea, CurrLoc), % CAREFUL, this disables any improvisation actions with effect "fromGeographicArea" or "toGeographicArea".
			fact(Road, swc:hasDoor, ps:oHatch_1),
			fact(ps:oHatch_1, swc:hasOpenCloseProperty, swc:closed),
			fact(ps:oHatch_1, swc:hasOpenCloseProperty, swc:openable)
		]),
		
	    % The door must not be open
        condition(false, [
	        fact(ps:oHatch_1, swc:hasOpenCloseProperty, swc:open)
	    ])
		
	]),
	effects([
	    % The door is now open
		condition(true, [
		    fact(Patiens, swc:hasOpenCloseProperty, swc:open)
		]),

		% The door is no longer closed
		condition(false, [
			fact(Patiens, swc:hasOpenCloseProperty, swc:closed)
		])
	])
]).*/

:- include('manipulate.pl').
:- include('transfer.pl').
:- include('transitMove.pl').