% ------------------------------------------
% FireCanon
% ------------------------------------------
% Goal for firing the canon and
% launching the canonball from the canon
% PRE:	there is a pirate, a canon, a canonball, a torch, the torch is lit
% SUC:	canon has been fired
% FAIL:	the torch is not lit

schema([
	head([
		type('http://www.owl-ontologies.com/StoryWorldSettings/Pirates_Jasper.owl#FireCanon'),
		agens(Agens), patiens(Patiens), target(Target), instrument(Instrument)
		]),
	class(goal),
	posPreconditions([
		(Agens, owlr:typeOrSubType, psj:'Pirate'),		% agens is a pirate,
		(Target, owlr:typeOrSubType, psj:'Canon'),		% a canon,
		(Patiens, owlr:typeOrSubType, psj:'CanonBall'),	% a canonball
		(Instrument, owlr:typeOrSubType, psj:'Torch')	% and a torch exist

		]),
	steps([
			[
				posSuccessConditions([ % dingen die waar zijn als het gelukt is 
					(Target, psj:hasFireProperty, psj:fired)]),		% canon has been fired
				posFailureConditions([ % dingen waardoor het goal faalt
					(Instrument, psj:hasLitProperty, psj:not_lit)])	% torch is not lit
			]
		])
	]).
	
% ------------------------------------------
% GoToCrowsNest
% ------------------------------------------
% Goal for climbing to the crow's nest
% PRE:	there is a pirate and a location of type crow's nest
% SUC:	the pirate is in the crow's nest

schema([
	head([
		type('http://www.owl-ontologies.com/StoryWorldSettings/Pirates_Jasper.owl#GoToCrowsNest'),
		agens(Agens), target(Target)
		]),
	class(goal),
	posPreconditions([
		(Agens, owlr:typeOrSubType, psj:'Pirate'),		% a pirate
		(Target, owlr:typeOrSubType, psj:'CrowsNest')	% and the crow's nest exists
		]),
	steps([
			[	posSuccessConditions([ 
					(Agens, swc:supportedBy, Target)])	% agens is at the crow's nest
			]
		])	
	]).	

% ------------------------------------------
% 	DEBUG COMMANDS
% ------------------------------------------
%	plan([(psj:oCanon_1, psj:hasFireProperty, psj:fired)], [], Plan).
%	plan([(psj:'RedBeard', swc:supportedBy, psj:oCrowsNest_1)], [], Plan).
%	impossible_operator(S,PosPrec,NegPrec).