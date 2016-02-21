  	
thread_schema([
	type(ps:'OnShip'),
	preconditions([
		condition(true, [
			rule(Cpt, owlr:typeOrSubType, fabula:'Character')
,			rule(Pir, owlr:typeOrSubType, fabula:'Character')
,			rule(Pir, owlr:isNot, Cpt)
,			rule(Treasure, owlr:typeOrSubType, ps:'TreasureChest')
%,			fact(Treasure, swc:supportedBy, IslandArea)
%,			fact(IslandArea, swc:partOfGeographicArea, ps:'oTreasureIsland_1')  
		])
	]),
%	settings([
		%condition(true, [
			%fact(ps:'Rum', swc:limited, true)
		%])
	%]),
	characters([
		character(Cpt)
,		character(Pir)
	]),
	location(ps:oDeck_1),
	goals([
		goal(Cpt, S)
,		goal(Pir, R)
	]),
	resolve_goals([
		goal(Cpt, T)
,		goal(Pir, U)
	])
]) :-
	goal_schema(S),
	schema_type(S, ps:'GetTreasure'),	% must be fully expanded
	schema_agens(S, Cpt),
	schema_patiens(S, Treasure),
	validate_schema(S, []),
	goal_schema(R),
	schema_type(R, ps:'GetTreasure'),	% must be fully expanded
	schema_agens(R, Pir),
	schema_patiens(R, Treasure),
	validate_schema(R, []),
	goal_schema(T),
	schema_type(T, ps:'Escape'),	% must be fully expanded
	schema_patiens(T, Treasure),
	schema_agens(T, Cpt),
	schema_target(T, Pir),
	goal_schema(U),
	schema_type(U, ps:'Escape'),	% must be fully expanded
	schema_agens(U, Pir),
	schema_patiens(U, Treasure),
	schema_target(U, Cpt).
	   	   	