  	
thread_schema([
	type(ps:'OnShip'),
	preconditions([
		condition(true, [
			rule(Cpt, owlr:typeOrSubType, fabula:'Character')
		])
	]),
	characters([
		character(Cpt)
	]),
	location(ps:oDeck_1),
	goals([
		goal(Cpt, S)
	])
]) :-
	goal_schema(S),
	schema_type(S, lolli:'GetBottle'),	% must be fully expanded
	schema_agens(S, Cpt),
	validate_schema(S, []).   	