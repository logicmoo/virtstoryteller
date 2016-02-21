
% :- episode(head(Vars), [pos-precs], [neg-precs], [code]).

thread_schema([
	type(ps:'OnShip'),
	preconditions([
		condition(true, [
			rule(P1, owlr:typeOrSubType, fabula:'Character'),
			rule(P2, owlr:typeOrSubType, fabula:'Character'),
			rule(P1, owlr:isNot, P2)
		])
	]),
	characters([
		character(P1),
		character(P2)
	]),
	location(ps:oDeck_1),
	goals([
		goal(P1, S)
	])
]) :-
	goal_schema(S)
,	schema_type(S, ps:'WoundEnemy')
,	schema_agens(S, P1)
,	schema_patiens(S, P2)
,	validate_schema(S, [])
.