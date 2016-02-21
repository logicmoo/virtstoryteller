% action database for pirates
% @author swartjes
% @date 26 nov 2007


action_schema([
	type(ps:'DrawWeapon'),
	arguments([	agens(Ag), patiens(Pa) ]),
	duration(1),
	preconditions([
		condition(true, [
			fact(Pa, swc:wornBy,		Ag),
			rule(Pa, owlr:typeOrSubType, ps:'Weapon'),
			rule(Ag, owlr:typeOrSubType, fabula:'Character')		
		])
	]),
	effects([
		condition(true, [
			fact(Pa,	swc:heldBy,	Ag)
		]),
		condition(false, [
			fact(Pa, swc:wornBy, Ag)			
		])
	])
]).

action_schema([
	type(ps:'Stab'),
	arguments([	agens(Ag), patiens(Pa), instrument(In) ]),
	duration(1),
	preconditions([
		% There is a rapier, held by Agens 
		condition(true, [
			rule(In, owlr:typeOrSubType, ps:'Rapier'),
			fact(In, swc:heldBy, Ag)
		])
	]),
	effects([
		condition(true, [
			fact(Pa,	ps:health, ps:wounded)
		])
	])
]).

:- include('manipulate.pl').
:- include('transfer.pl').
:- include('transitMove.pl').



