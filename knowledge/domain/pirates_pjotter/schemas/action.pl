% action database for pirates
% @author tommassen
% @date 26 nov 2007


action_schema([
	type(ps:'DrinkRum'),
	arguments([agens(Ag), patiens(Pa)]),
	duration(1),
	preconditions([
		condition(true, [
		    rule(Ag, swcr:knowsAction, fabula:'DrinkRum')
	    ]),
		condition(true, [
		    rule(Pa, owlr:typeOrSubType, ps:'RumBottle')
		]),
		condition(true, [
		    fact(Pa, swc:heldBy, Ag)
		]),
		condition(true, [
		    fact(Pa, swc:contains, ps:'Rum')	   
		])
	]),
    effects([
	    condition(true, [
			fact(Ag, swc:hasState, ps:'Drunk')
		]),
		condition(false, [
    		fact(Pa, swc:contains, ps:'Rum')
		])
	])
]).

action_schema([
	type(ps:'OpenHatch'),
    arguments([    agens(Agens), patiens(ps:oHatch_1), road(Road) ]),
    duration(1),
    preconditions([
    	
    	 % Agens can do the action
		condition(true, [
			rule(Agens,	swcr:knowsAction, fabula:'OpenDoor')
		]),
		
		% Instrument is located "at" a door that is openable, but closed.
		condition(true, [
			fact(Agens, swc:supportedBy, CurrLoc),
			fact(Road, swc:connectedToGeographicArea, CurrLoc), % CAREFUL, this disables any improvisation actions with effect "fromGeographicArea" or "toGeographicArea".
			fact(Road, swc:hasDoor, ps:oHatch_1),
			fact(ps:oHatch_1, swc:hasOpenCloseProperty, swc:closed),
			fact(ps:oHatch_1, swc:hasOpenCloseProperty, swc:openable)
		]),
		
	    % The door must not be open
        condition(false, [
	        fact(ps:oHatch_1, swc:hasOpenCloseProperty, swc:open)
	    ])
		
	]),
	effects([
	    % The door is now open
		condition(true, [
		    fact(Patiens, swc:hasOpenCloseProperty, swc:open)
		]),

		% The door is no longer closed
		condition(false, [
			fact(Patiens, swc:hasOpenCloseProperty, swc:closed)
		])
	])
]).

:- include('manipulate.pl').
:- include('transfer.pl').
:- include('transitMove.pl').
:- include('ship_actions.pl').