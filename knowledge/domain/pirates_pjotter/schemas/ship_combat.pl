action_schema([
	type(ps:'BoardShip'),
	arguments([agens(Agens), target(Target), instrument(OwnShip) ]),
	duration(1),
	preconditions([
		condition(true, [
			fact(Target, ps:broken, _),
			rule(TargetDeck, owlr:typeOrSubType, ps:'Deck'),
			fact(Target, swc:hasRegion, TargetDeck)
		]),
		condition(false, [
			fact(OwnShip, ps:mooredAt, _)
		]),
		condition(true, [
			rule(Deck, owlr:typeOrSubType, ps:'Deck'),
			fact(Agens, swc:supportedBy, Deck),
			fact(OwnShip, swc:hasRegion, Deck)
		]),
		condition(true, [
			rule(Target, owlr:typeOrSubType, ps:'Ship'),
			rule(OwnShip, owlr:typeOrSubType, ps:'Ship')
		])
	]),
	effects([
		condition(false, [
			fact(Agens, swc:supportedBy, Deck)
		]),
		condition(true, [
			fact(Agens, swc:supportedBy, TargetDeck),
			fact(Agens, ps:returnBoard, Deck),
			fact(Agens, ps:boardedTo, TargetDeck)
		])
	])
]).

action_schema([
	type(ps:'ReboardShip'),
	arguments([agens(Agens), target(OwnDeck), from(Deck) ]),
	duration(1),
	preconditions([
		condition(true, [
			fact(Agens, ps:boardedTo, Deck),
			fact(Agens, ps:returnBoard, OwnDeck),
			fact(Agens, swc:supportedBy, Deck)
		])
	]),
	effects([
		condition(false, [
			fact(Agens, swc:supportedBy, Deck),
			fact(Agens, ps:boardedTo, Deck),
			fact(Agens, ps:returnBoard, OwnDeck)
		]),
		condition(true, [
			fact(Agens, swc:supportedBy, OwnDeck)
		])
	])
]).

action_schema([
	type(ps:'ShootCannon'),
	arguments([agens(Agens), instrument(Cannon), target(Target) ]),
	duration(1),
	preconditions([
		condition(true, [
			rule(Cannon, owlr:typeOrSubType, ps:'Cannon')
		]),
		condition(true, [
			fact(Cannon, swc:supportedBy, Loc),
			fact(Agens, swc:supportedBy, Loc)
		]),
		condition(false, [
			fact(Target, ps:mooredAt, _)
		])
	]),
	effects([
		condition(true, [
			fact(Target, ps:'broken', Cannon)
		])
	])
]).



action_schema([
	type(ps:'ShootCannonMooredShip'),
	arguments([agens(Agens), instrument(Cannon), target(Target) ]),
	duration(1),
	preconditions([
		condition(true, [
			rule(Cannon, owlr:typeOrSubType, ps:'Cannon')
		]),
		condition(true, [
			fact(Cannon, swc:supportedBy, Loc),
			fact(Agens, swc:supportedBy, Loc),
			fact(Target, ps:mooredAt, Moor)
		])
	]),
	effects([
		condition(true, [
			fact(Target, ps:'broken', Cannon)
		]),
		condition(false, [
			fact(Target, ps:mooredAt, Moor)
		])
	])
]).
