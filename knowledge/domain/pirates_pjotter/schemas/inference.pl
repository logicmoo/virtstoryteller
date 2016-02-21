inference_schema([
	type(fabula:'MakeStuckIsland'),
	arguments([agens(Agens), patiens(Patiens)] ),
	
	preconditions([
		condition(true, [
			rule(Patiens, owlr:typeOrSubType, fabula:'Character'),
			fact(Patiens, swc:supportedBy, Loc),
			fact(Loc, swc:partOfGeographicArea, ps:'oTreasureIsland_1'),
			fact(fabula:'story_phase', is, fabula:'rising_action')
		]),
		condition(false, [
			fact(ps:'oShip_1', ps:'mooredAt', ps:'oSea_1')
		]),
		condition(false, [
			fact(ps:'oShip_2', ps:'mooredAt', ps:'oSea_1')			
		])
	]),
	effects([
		condition(true, [
			fact(Patiens, ps:stuck, ps:'oTreasureIsland_1')
		])
	])
]).
			
inference_schema([
	type(fabula:'MakeStuckBoat'),
	arguments([agens(Agens), patiens(Patiens)] ),
	
	preconditions([
		condition(true, [
			rule(Patiens, owlr:typeOrSubType, fabula:'Character'),
			fact(Patiens, swc:supportedBy, Deck),
			rule(Deck, owlr:typeOrSubType, ps:'Deck'),
			fact(Ship,swc:hasRegion,Deck),
			rule(Ship, owlr:typeOrSubType, ps:'Ship'),
			fact(Ship, ps:broken, _)
		])/*,
		condition(false, [
			fact(ps:'oShip_1', ps:'mooredAt', ps:'oSea_1')
		]),
		condition(false, [
			fact(ps:'oShip_2', ps:'mooredAt', ps:'oSea_1')
		])*/
	]),
	effects([
		condition(true, [
			fact(Patiens, ps:stuck, Ship)
		])
	])
]).
