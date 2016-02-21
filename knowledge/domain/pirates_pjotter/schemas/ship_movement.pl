action_schema([
	type(ps:'SetSail'),
	arguments([agens(Agens), instrument(Ship), target(Target) ]),
	duration(2),
	preconditions([
		% And which is moored somewhere
		condition(true, [
			fact(Ship, ps:mooredAt, Target)
		]),
		% and the ship is not broken
		condition(false, [
			fact(Ship, ps:'broken', _)
		]),
		% Instrument is a ship
		condition(true, [
			rule(Ship, owlr:typeOrSubType, ps:'Ship')
		]),
		condition(true, [
			rule(Deck, owlr:typeOrSubType, ps:'Deck'),
			fact(Agens, swc:supportedBy, Deck),
			fact(Ship, swc:hasRegion, Deck)
		])
	]),
	effects([
		% Ship is at target
		condition(false, [
			fact(Ship, ps:mooredAt, Target)
		])
	])
]).

action_schema([
	type(ps:'SailShip'),
	arguments([agens(Agens), target(Target), instrument(Ship) ]),
	duration(2),
	preconditions([
		% Target is a land one can sail to
		condition(true, [
			rule(Target, owlr:typeOrSubType, ps:'MooringArea')
		]),
		% Instrument is a ship
		condition(true, [
			rule(Ship, owlr:typeOrSubType, ps:'Ship')
		]),
		% And which is not moored somewhere
		condition(false, [
			fact(Ship, ps:mooredAt, _)
		]),		
		condition(false, [
			fact(Ship, ps:'broken', _)
		]),	
		condition(true, [
			rule(Deck, owlr:typeOrSubType, ps:'Deck'),
			fact(Agens, swc:supportedBy, Deck),
			fact(Ship, swc:hasRegion, Deck)
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
	type(ps:'GetOnBoat'),
	arguments([ agens(Agens), location(Deck), target(Ship) ]),
	duration(1),
	preconditions([
		condition(true, [
			rule(Deck, owlr:typeOrSubType, ps:'Deck'),
			fact(Ship, swc:hasRegion, Deck)
		]),
		% The boat is moored somewhere
		condition(true, [
			fact(Ship, ps:mooredAt, Land),
			fact(Agens, swc:supportedBy, Land)
		]),
		condition(false, [
			fact(Agens, ps:stuck, _)
		])	
	]),
	effects([
		% I am on the deck of the boat
		condition(true, [
			fact(Agens, swc:supportedBy, Deck)
		]),
		% I am now on the land
		condition(false, [
			fact(Agens, swc:supportedBy, Land)
		])
	])
]).



action_schema([
	type(ps:'GetOffBoat'),
	arguments([ agens(Agens), location(Deck), target(Land) ]),
	duration(1),
	preconditions([
		% The boat is moored somewhere
		condition(true, [
			fact(Ship, ps:mooredAt, Land)
		]),
		% I am on the deck of the boat
		condition(true, [
			rule(Deck, owlr:typeOrSubType, ps:'Deck'),
			fact(Agens, swc:supportedBy, Deck),
			fact(Ship, swc:hasRegion, Deck)
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
