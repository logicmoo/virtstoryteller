
% :- episode(head(Vars), [pos-precs], [neg-precs], [code]).
   	
thread_schema([
	type(lolli:'BuyingIce'),
	preconditions([
		condition(true, [
			rule(Kid, owlr:typeOrSubType, lolli:'Kid'),
			rule(Seller, owlr:typeOrSubType, lolli:'IceVendor'),
			fact(Kid, lolli:at, Location)
		])
	]),
	characters([
		character(Kid),
		character(Seller)
	]),
	location(Location),
	settings([
		condition(true, [
			% Backstory: the kid was greedy
			fabula(lolli:greedy, rdf:type, lolli:'Greedy'),
			fabula(lolli:greedy, fabula:character, Kid)
		])
	]),
	goals([
		goal(Kid, S)
	])
]) :-
	goal_schema(S),
%	schema_type(S, 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#HaveIce'),	% must be fully expanded
	schema_type(S, lolli:'HaveIce'),	% must be fully expanded
	schema_agens(S, Kid),
	validate_schema(S, []).
		
		
		