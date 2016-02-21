% Action database for the pirates_jasper2 domain
% @author Jasper Bragt
% @date Aug 2008

% psj2:FireCanon
% psj2:LoadCanon
% psj2:GoTo
% psj2:GoToDoor
% fabula:TakeFrom
% fabula:OpenDoor

% 'FireCanon'
% Action semantics: AGENS fires PATIENS from TARGET with INSTRUMENT
action_schema([
	type(psj2:'FireCanon'),
	arguments([agens(Agens), patiens(Patiens), target(Target), instrument(Instrument), location(Location)]),
	duration(1),
	preconditions([
		condition(true, [
			%rule(Patiens, owlr:typeOrSubType, psj2:'CanonBall'),% the canonball
			fact(Target, swc:contains, Patiens),				% is in
			rule(Target, owlr:typeOrSubType, psj2:'Canon'),		% the canon
			fact(Agens, swc:has, Instrument),					% the pirate has
			rule(Instrument, owlr:typeOrSubType, psj2:'Torch'),	% the torch
			fact(Instrument, swc:hasAttribute, psj2:lit),		% which is lit
			fact(Agens, swc:at, Location),						% and is located
			fact(Target, swc:at, Location)						% next to the canon 
		])
	]),
	effects([
		% Add emotion effects? (e.g. Agens is very happy about what he just did)
		condition(true, [
			fact(Target, swc:hasAttribute, psj2:fired)
		]),
		condition(false, [
			fact(Target, swc:contains, Patiens)	% the canonball is no longer in the canon (but where is it?)
		])
	])
]).

% 'LoadCanon'
% Action semantics: AGENS loads PATIENS with INSTRUMENT
action_schema([
	type(psj2:'LoadCanon'),
	arguments([agens(Agens), patiens(Patiens), instrument(Instrument), location(Location)]),
	duration(1),
	preconditions([
		condition(true, [
		rule(Patiens, owlr:typeOrSubType, psj2:'Canon'),		% there is a canon,
		fact(Agens, swc:has, Instrument),						% the pirate has
	%	rule(Instrument, owlr:typeOrSubType, psj:'CanonBall'),	% the canonball
		fact(Agens, swc:at, Location),							% and is located
		fact(Patiens, swc:at, Location)							% next to the canon 
		])
	]),
	effects([
		condition(true, [
			fact(Patiens, swc:contains, Instrument)
		]),
		condition(false, [
			fact(Agens, swc:has, Instrument)		% the agens no longer has the canonball
		])
	])
]).

% -------------------------------------------
% GoTo
% -------------------------------------------

% Agens walks from current Location to Target via Instrument
action_schema([
	type(psj2:'GoTo'),
	arguments([	agens(Agens), target(Target), location(CurLoc), instrument(Road)]),
	duration(1),
	preconditions([
	    % Agens can do the action, either with his own body or using an instrument that can do the action
%		condition(true, [
%			fact(Agens,    swcr:knowsAction,       fabula:'WalkFromTo')
%		]),
		
		% There is a road to walk on, from where the instrument is
		condition(true, [
			fact(Agens,	swc:at, CurLoc),
			fact(Road,	swc:from, CurLoc),
			rule(Road,	owlr:typeOrSubType,     swc:'Path'), 
			fact(Road,	swc:to,   Target)
		]),
		
	    % There is no door on this path
		condition(false, [
			fact(Road, swc:hasDoor, _)   
		])
	]),
	effects([
	    % We are now at the target location
		condition(true, [
			fact(Agens,	swc:at,		Target)
		]),

	    % We are no longer at our current location
	    condition(false, [
			fact(Agens,	swc:at,		CurLoc)
		])
	])
]).

% --------------------------------------------------------------------
% GoToDoor -- walking on a road that contains a door, "upstream"
% --------------------------------------------------------------------

% Agens walks from Location to Target through Door via Instrument
action_schema([
	type(psj2:'GoToDoor'),
	arguments([ agens(Agens), location(Target), location(CurLoc), instrument(Road), door(Door) ]),
	duration(1),
	preconditions([
	    % Agens can do the action, either with his own body or using an instrument that can do the action		
%		condition(true, [
%			rule(Agens,	swcr:knowsAction, fabula:'WalkFromToDoor')
%		]),
		
		% There is an "upstream" road to walk on, from where the instrument is		
		condition(true, [
			rule(Road, 	owlr:typeOrSubType, swc:'Path'),
			fact(Road,	swc:to, Target),
			fact(Road,	swc:from, CurLoc)
		]),
		
		% Agens is at the start of the path
		condition(true, [
			fact(Agens,	swc:at,	CurLoc)
		]),
		
		% There is a door on this road, which is open
		condition(true, [
			fact(Road,    swc:hasDoor,    Door),
			fact(Door,    swc:hasAttribute, swc:open)
		])
	]),
	effects([
	    % We are now at the target location	
		condition(true, [
			fact(Agens,	swc:at,		Target)
		]), 
		
	    % We are no longer at our current location		
	    condition(false, [
			fact(Agens,	swc:at,		CurLoc)			
		])
	])
]).

% ---- Agens takes Patiens from Target ----

action_schema([
	type(fabula:'TakeFrom'),
	arguments([	agens(Agens), patiens(Patiens), location(Target) ]),
	duration(1),
	preconditions([
		% Agens can do the action
%		condition(true, [
%		    rule(Agens,	swcr:knowsAction,	fabula:'TakeFrom')
%		]),
		% Both at same location
		condition(true, [
		    fact(Patiens, swc:at, Target),
		    fact(Agens,	swc:at,	Target)		
		]),
		% Can't take yourself
		condition(false, [
		    rule(Agens,	owlr:is, Patiens)		  
		])
	]),
	effects([
		% Agent now has the thing
		condition(true, [
		    fact(Agens,	swc:has, Patiens)		   
		]),
		% Thing is no longer where it was
		condition(false, [
		    fact(Patiens, swc:at, Target)
		])
	])
]).

% -------------------------------------------------
% OpenDoor -- opening something that can be opened
% -------------------------------------------------
action_schema([
	type(fabula:'OpenDoor'),
    arguments([    agens(Agens), patiens(Patiens), road(Road) ]),
    duration(1),
    preconditions([
    	
    	 % Agens can do the action
%		condition(true, [
%			rule(Agens,	swcr:knowsAction, fabula:'OpenDoor')
%		]),
		
		% Instrument is located "at" a door that is openable, but closed.
		condition(true, [
			fact(Agens, swc:at, CurrLoc), 
			fact(Road, swc:from, CurrLoc), % CAREFUL, this disables any improvisation actions with effect "fromGeographicArea" or "toGeographicArea".
			fact(Road, swc:hasDoor, Patiens),
			fact(Patiens, swc:hasAttribute, swc:closed)
			%fact(Patiens, swc:hasOpenCloseProperty, swc:openable)
		]),
		
	    % The door must not be open
        condition(false, [
	        fact(Patiens, swc:hasAttribute, swc:open)
	    ])
		
	]),
	effects([
	    % The door is now open
		condition(true, [
		    fact(Patiens, swc:hasAttribute, swc:open)
		]),

		% The door is no longer closed
		condition(false, [
			fact(Patiens, swc:hasAttribute, swc:closed)
		])
	])
]).
