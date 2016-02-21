% Een thread heeft precondities, en definieert setting information, die variabelen kan bevatten.
thread_schema([
	type(ps:'OnShip'),
	preconditions([
		condition(true, [
			rule(Cpt, owlr:typeOrSubType, fabula:'Character'),
			rule(Cpt2, owlr:typeOrSubType, fabula:'Character'),
			rule(Cpt, owlr:isNot, Cpt2)
		])
	]),
	characters([
		character(Cpt),
		character(Cpt2)
	]),
	location(ps:oDeck_1),
	goals([
		goal(Cpt, S),
		goal(Cpt2, T)
	])
	
]) :-
	goal_schema(S),
	schema_type(S, ps:'GetDrunk'),	% must be fully expanded
	schema_agens(S, Cpt),
	goal_schema(T),
	schema_type(T, ps:'RefillWaterSupply'),
	schema_agens(T, Cpt2).   


		
		