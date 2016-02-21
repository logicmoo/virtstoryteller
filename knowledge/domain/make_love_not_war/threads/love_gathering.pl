
% :- episode(head(Vars), [pos-precs], [neg-precs], [code]).
   	
thread_schema([
	type(love:'LoveGathering'),
	preconditions([
		condition(true, [
			rule(A, owlr:typeOrSubType, fabula:'Character'),
			rule(B, owlr:typeOrSubType, fabula:'Character'),
			rule(C, owlr:typeOrSubType, fabula:'Character'),
			rule(A, owlr:isNot, B),
			rule(B, owlr:isNot, C),
			rule(A, owlr:isNot, C)
		])
	]),
	characters([
		character(A),
		character(B),
		character(C)
	])
]).