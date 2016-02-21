% action database for pirates
% @author swartjes
% @date 26 nov 2007

:- include('transitMove.pl').
:- include('transfer.pl').
:- include('manipulate.pl').

schema([
	head([
		type('http://www.owl-ontologies.com/FabulaKnowledge.owl#DrinkRum'),
		agens(Ag), patiens(Pa)
		]),
	class(action),
	duration(1),
	posPreconditions([
		(Pa, 'http://www.owl-ontologies.com/StoryWorldCore.owl#heldBy',		Ag),
		(Ag, swcr:knowsAction, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#DrinkRum'),
		(Pa, rdf:type, ps:'RumBottle'),
		(Pa, swc:contains, ps:'Rum')
		]),
	negPreconditions([
		]),
	posEffects([
		(Ag,	'http://www.owl-ontologies.com/StoryWorldCore.owl#hasState',	ps:'Drunk')
		]),
	negEffects([
		(Pa, swc:contains, ps:'Rum')
		])
]).


schema([
	head([
		type('http://www.owl-ontologies.com/FabulaKnowledge.owl#DrinkRumTEST'),
		agens(Ag), patiens(Pa)
		]),
	class(action),
	duration(1),
	posPreconditions([]),
	negPreconditions([
			(ps:leChuck, rdf:type, ps:'Pirate')
		]),
	posEffects([
		(Ag,	'http://www.owl-ontologies.com/StoryWorldCore.owl#hasState',	ps:'Drunk')
		]),
	negEffects([
		(Pa, swc:contains, ps:'Rum')
		])
]).