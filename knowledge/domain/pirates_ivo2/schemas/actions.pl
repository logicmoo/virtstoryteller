% action database for pirates
% @author swartjes
% @date 26 nov 2007

% -----------------------
% Draw your weapon
% -----------------------
action_schema([
	type(ps:'DrawWeapon'),
	arguments([	agens(Ag), patiens(Pa) ]),
	duration(1),
	preconditions([
		condition(true, [
			fact(Pa, swc:wornBy,		Ag),
			rule(Pa, owlr:typeOrSubType, ps:'Weapon'),
			rule(Ag, owlr:typeOrSubType, fabula:'Character')		
		])
	]),
	effects([
		condition(true, [
			fact(Pa,	swc:heldBy,	Ag)
		]),
		condition(false, [
			fact(Pa, swc:wornBy, Ag)			
		])
	])
]).

% -----------------------------
% Stab someone with your rapier
% -----------------------------
action_schema([
	type(ps:'Stab'),
	arguments([	agens(Ag), patiens(Pa), instrument(In) ]),
	duration(1),
	preconditions([
		condition(true, [
			fact(In, swc:heldBy, Ag),
			rule(In, owlr:typeOrSubType, ps:'Rapier'),
			fact(Ag, swc:supportedBy, Loc),
			fact(Pa, swc:supportedBy, Loc)
		])
	]),
	effects([
		condition(true, [
			fact(Pa,	ps:health, ps:wounded)
		])
	])
]).

% -----------------------
% Sail to a nearby island
% -----------------------
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

% ---------
% Go ashore
% ---------
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

% ----------------
% Go aboard a ship
% ----------------
action_schema([
	type(ps:'GoAboard'),
	arguments([ agens(Agens), location(Deck), target(Ship) ]),
	duration(1),
	preconditions([
		% I am now on the land
		condition(true, [
			fact(Agens, swc:supportedBy, Land)
		]),
		% The boat is moored at this land
		condition(true, [
			fact(Ship, ps:mooredAt, Land)
		]),
		% The boat has a deck
		condition(true, [
			rule(Deck, owlr:typeOrSubType, ps:'Deck'),
			fact(Ship, swc:hasRegion, Deck)
		])
	]),
	effects([
		% I am now on the land
		condition(true, [
			fact(Agens, swc:supportedBy, Deck)
		]),
		% And no longer on the land
		condition(false, [
			fact(Agens, swc:supportedBy, Land)
		])
	])
]).

% ---------------------------------------------
% Fill the water supply of your boat with water
% ---------------------------------------------
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

% ---------------------------------------------
% Say: "Arr, let's get some rum"
% ---------------------------------------------
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

% ---------------------------------------------
% Say: "Prepare to die!"
% ---------------------------------------------
action_schema([
	type(ps:'SayEnGarde'),
	arguments([	agens(Agens), patiens(Patiens) ]),
	duration(1),
	preconditions([
		% Dialog partners must be characters
		condition(true, [
			rule(Agens, owlr:typeOrSubType, fabula:'Character'),
			rule(Patiens, owlr:typeOrSubType, fabula:'Character')
		]),
		% And we're at the same location (i.e., the confrontation is there)
		condition(true, [
			fact(Agens, swc:supportedBy, Loc),
			fact(Patiens, swc:supportedBy, Loc)
		])
	]),
	effects([])
]).

% ---------------------------------------------
% Say: "No way!"
% ---------------------------------------------
action_schema([
	type(ps:'SayNoWay'),
	arguments([	agens(Agens), patiens(Patiens) ]),
	duration(1),
	preconditions([
		% Dialog partners must be characters
		condition(true, [
			rule(Agens, owlr:typeOrSubType, fabula:'Character'),
			rule(Patiens, owlr:typeOrSubType, fabula:'Character')
		]),
		% And we're at the same location (i.e., the confrontation is there)
		condition(true, [
			fact(Agens, swc:supportedBy, Loc),
			fact(Patiens, swc:supportedBy, Loc)
		])			
	]),
	effects([])
]).

% ----------
% Drink rum
% ----------
action_schema([
	type(ps:'DrinkRum'),
	arguments([agens(Ag), patiens(Pa)]),
	duration(1),
	preconditions([
	    % There must be a rum bottle
		condition(true, [
		    rule(Pa, owlr:typeOrSubType, ps:'Bottle')
		]),
		% I must be holding the rum bottle
		condition(true, [
		    fact(Pa, swc:heldBy, Ag)
		]),
		% The rum bottle must contain rum
		condition(true, [
		    fact(Pa, swc:contains, Rum),
		    rule(Rum, owlr:typeOrSubType, ps:'Rum')
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


:- include('actions_manipulate.pl').
:- include('actions_transfer.pl').
:- include('actions_transitMove.pl').