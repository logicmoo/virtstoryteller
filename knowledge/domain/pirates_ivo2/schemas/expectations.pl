% Expectation schemas define what might be expected given the current context
%	e.g. seeing someone carrying a sword might lead to the expectation that they are gonna stab you

% If someone holds a weapon, you might believe they will attack you
expectation_schema([
	type(ps:'AboutToBeAttacked'),
	arguments([	agens(Agens), patiens(Char), instrument(Weapon) ]),
	preconditions([
		% Agens holds a rapier
		condition(true, [
			rule(Weapon, owlr:typeOrSubType, ps:'Rapier'),
			fact(Weapon, ps:heldBy, Agens)
		]),
		% Agens at same location as other character		
		condition(true, [
			rule(Char, owlr:typeOrSubType, swc:'Character'),
			rule(Char, owlr:isNot, Agens),
			fact(Char, swc:supportedBy, Loc),
			fact(Agens, swc:supportedBy, Loc)
		])
	]),
	effects([
		% There is (gonna be) a stab action
		condition(true, [			
			fabula(Act, owlr:typeOrSubType, ps:'Stab'),
			fabula(Act, fabula:agens, Agens),
			fabula(Act, fabula:patiens, Char)
		])
	])
]).
  
	
	
