expectation_schema([
	type(ps:'ExpectToFollow'),
	arguments([agens(Agens), patiens(Patiens), location(Loc)]),
	preconditions([
		condition(true, [
			rule(Loot, owlr:typeOrSubType, ps:'TreasureChest'),
			fact(Loot, swc:heldBy, Agens),
			rule(Patiens, owlr:typeOrSubType, fabula:'Character'),
			rule(Agens, owlr:isNot, Patiens),
			rule(Patiens, owlr:typeOrSubType, fabula:'Character'),
			fact(Agens, swc:supportedBy, Loc)			
		])
	]),
	effects([
		condition(true, [
			fact(Patiens, swc:supportedBy, Loc)
		])
	])
]).

expectation_schema([
	type(ps:'ExpectToSetSail'),
	arguments([agens(Agens), patiens(Patiens), location(Loc)]),
	preconditions([
		condition(false, [
			fact(Ship, ps:'broken', _)
		]),
		condition(true, [
			rule(Deck, owlr:typeOrSubType, ps:'Deck'),
			fact(Patiens, swc:supportedBy, Deck),
			fact(Ship, swc:hasRegion, Deck),
			fact(Ship, ps:mooredAt, Loc),
			rule(Patiens, owlr:typeOrSubType, fabula:'Character'),
			rule(Agens, owlr:isNot, Patiens)
		])
		
	]),
	effects([
		condition(false, [
			fact(Ship, ps:mooredAt, Loc)
		])
	])
]).

% if someone is on a ship, expect him to sail
expectation_schema([
	type(ps:'ExpectToSail'),
	arguments([agens(Agens), patiens(Patiens), location(Loc)]),
	preconditions([
		condition(false, [
			fact(Ship, ps:mooredAt, _)
		]),
		condition(false, [
			fact(Ship, ps:'broken', _)
		]),		
		condition(true, [
			rule(Deck, owlr:typeOrSubType, ps:'Deck'),
			fact(Patiens, swc:supportedBy, Deck),
			fact(Ship, swc:hasRegion, Deck),
			rule(Loc, owlr:typeOrSubType, ps:'MooringArea'),
			rule(Patiens, owlr:typeOrSubType, fabula:'Character'),
			rule(Agens, owlr:isNot, Patiens)			
		])
		
	]),
	effects([
		condition(true, [
		 	fact(Ship, ps:mooredAt, Loc)		 	
		])
	])
]).

/*
expectation_schema([
	type(ps:'ExpectToLeaveShip'),
	arguments([agens(Agens), patiens(Patiens), location(Loc)]),
	preconditions([
		condition(true, [
			rule(Deck, owlr:typeOrSubType, ps:'Deck'),
			fact(Patiens, swc:supportedBy, Deck),
			fact(Ship, swc:hasRegion, Deck),
			fact(Ship, ps:mooredAt, Loc),
			rule(Patiens, owlr:typeOrSubType, fabula:'Character'),
			rule(Agens, owlr:isNot, Patiens)
		])
	]),
	effects([
		condition(true, [
		 	fact(Patiens, swc:supportedBy, Loc)
		]),
		condition(false, [
			fact(Patiens, swc:supportedBy, Deck)
		])
	])
]).


*/