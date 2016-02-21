  	
thread_schema([
	type(psj2:'On_Ship'),
	preconditions([
		condition(true, [
			rule(Cpt, owlr:typeOrSubType, fabula:'Character')%,
			%rule(Cpt2, owlr:typeOrSubType, fabula:'Character'),
			%rule(Cpt, owlr:isNot, Cpt2)
		])
	]),
	characters([
		character(Cpt)%,
		%character(Cpt2)
	]),
	location(psj2:oDeck_1)

%	goals([
%		goal(Cpt, S)
%	])
]).	
%]) :-
%	goal_schema(S),
%	schema_type(S, psj2:'GatherBottles'),	% must be fully expanded
%	schema_agens(S, Cpt).   


		
		