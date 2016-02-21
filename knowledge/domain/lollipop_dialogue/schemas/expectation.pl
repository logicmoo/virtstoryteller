% Expectation schemas

% If you greet someone, they will greet back
expectation_schema([
	type(lolli:'ExpectToBeGreetedBack'),
	arguments([agens(Agens), patiens(Patiens)]),
	preconditions([
		% Agens has greeted patiens
		condition(true, [
            fact(Agens, lolli:greeted, Patiens)
		]),		
		% Patiens has not greeted back yet
		condition(false, [
			fact(Patiens, lolli:greeted, Agens)
		]),
		% And at same location
		condition(true, [
            fact(Agens, lolli:at, Location),
            fact(Patiens, lolli:at, Location)
		])		
	]),
	effects([
		% You can expect to be greeted back
		condition(true, [
		 	fact(Patiens, lolli:greeted, Agens)
		])
	])
]).


% AGENS expects that TARGET will sell him/her PATIENS
% If you asked for something, and paid them for it, they will sell it.
expectation_schema([
	type(lolli:'ExpectToBeSold'),
	arguments([agens(Agens), patiens(Patiens), target(Target)]),
	preconditions([
		% Target (still) has patiens
		condition(true, [
			fact(Target, lolli:has, Patiens)
		]),	
		% Agens has asked target for patiens
		condition(true, [
            fabula(Act, rdf:type, lolli:'AskFor'),
            fabula(Act, fabula:agens, Agens),
            fabula(Act, fabula:patiens, Patiens),
            fabula(Act, fabula:target, Target)
		]),
		% Agens has paid target
		condition(true, [
			fact(Agens, lolli:paid, Target)
		])
		
	]),
	effects([
		% You can expect to have it, and for target to NOT have it anymore
		condition(true, [
			fact(Agens, lolli:has, Patiens)
		]),
		condition(false, [
			fact(Target, lolli:has, Patiens)
		])
	])
]).

% AGENS expects that TARGET will pay him/her (with PATIENS)
expectation_schema([
	type(lolli:'ExpectToBePaid'),
	arguments([agens(Agens), patiens(Patiens), target(Target)]),
	preconditions([
		% Target (still) has money
		condition(true, [
			fact(Target, lolli:has, Patiens),
			fact(Patiens, rdf:type, lolli:'Money')
		]),	
		% Agens has asked target for the money
		condition(true, [
            fabula(Act, rdf:type, lolli:'AskFor'),
            fabula(Act, fabula:agens, Agens),
            fabula(Act, fabula:patiens, Patiens),
            fabula(Act, fabula:target, Target)
		])
		
	]),
	effects([
		% You can expect to have it, and for target to NOT have it anymore
		condition(true, [
			fact(Agens, lolli:has, Patiens)
		]),
		condition(true, [
			fact(Target, lolli:paid, Agens)
		]),
		condition(false, [
			fact(Target, lolli:has, Patiens)
		])
	])
]).