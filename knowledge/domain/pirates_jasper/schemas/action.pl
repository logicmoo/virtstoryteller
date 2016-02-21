% Action database for the pirates_jasper domain
% Jasper Bragt
% Feb 2008

:- include('transitMove.pl').
:- include('transfer.pl').
:- include('manipulate.pl').

% 'Fire'
% Action semantics: AGENS fires PATIENS from TARGET with INSTRUMENT
schema([
	head([
		type('http://www.owl-ontologies.com/StoryWorldSettings/Pirates_Jasper.owl#Fire'),
		agens(Agens), patiens(Patiens), target(Target), instrument(Instrument), location(Location)
		]),
	class(action),
	duration(1),
	posPreconditions([
		(Patiens, owlr:typeOrSubType, psj:'CanonBall'),	% the canonball
		(Patiens, swc:containedBy, Target),				% is in
		(Target, owlr:typeOrSubType, psj:'Canon'),		% the canon
		(Instrument, swc:heldBy, Agens),				% the pirate has
		(Instrument, owlr:typeOrSubType, psj:'Torch'),	% the torch
		(Instrument, psj:hasLitProperty, psj:lit),		% which is lit
		(Agens, swc:supportedBy, Location),				% and is located
		(Target, swc:supportedBy, Location)				% next to the canon 
		
		]),
	negPreconditions([
		]),
	posEffects([
		% Add emotion effects? (e.g. Agens is very happy about what he just did)
		(Target, psj:hasFireProperty, psj:fired)
		]),
	negEffects([
		(Patiens, swc:containedBy, Target)	% the canonball is no longer in the canon (but where is it?)
		])
	]).

% 'Load'
% Action semantics: AGENS loads PATIENS with INSTRUMENT
schema([
	head([
		type('http://www.owl-ontologies.com/StoryWorldSettings/Pirates_Jasper.owl#Load'),
		agens(Agens), patiens(Patiens), instrument(Instrument), location(Location)
		]),
	class(action),
	duration(1),
	posPreconditions([
		(Patiens, owlr:typeOrSubType, psj:'Canon'),			% there is a canon,
		(Instrument, swc:heldBy, Agens),					% the pirate has
		(Instrument, owlr:typeOrSubType, psj:'CanonBall'),	% the canonball
		(Agens, swc:supportedBy, Location),					% and is located
		(Patiens, swc:supportedBy, Location)				% next to the canon 
		
		]),
	negPreconditions([
		]),
	posEffects([
		(Instrument, swc:containedBy, Patiens)
		]),
	negEffects([
		(Instrument, swc:heldBy, Agens)		% the agens no longer has the canonball
		])
	]).