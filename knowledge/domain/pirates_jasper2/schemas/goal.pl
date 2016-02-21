% Goal database for the Pirates_Jasper2 domain
% @author Jasper Bragt
% @date Aug 2008

% -----------------------------------------------------------

% ------------------------------------------
% LaunchCanon
% ------------------------------------------
% Goal for firing the canon and
% launching the canonball from the canon
% PRE:	there is a pirate, a canon, a canonball, a torch, the torch is lit
% SUC:	canon has been fired

goal_schema([
	type(psj2:'LaunchCanon'),
	arguments([agens(Agens), patiens(Patiens), target(Target), instrument(Instrument)]),
	preconditions([
	    condition(true, [
			fact(Agens, swc:hasRole, psj2:'Pirate'),			% agens is a pirate,
			rule(Target, owlr:typeOrSubType, psj2:'Canon'),		% a canon,
			rule(Patiens, owlr:typeOrSubType, psj2:'CanonBall'),% a canonball
			rule(Instrument, owlr:typeOrSubType, psj2:'Torch'),	% and a torch exist
			fact(Instrument, swc:hasAttribute, psj2:lit)		% which is lit
		])
	]),
	success_conditions([
        condition(true, [
			fact(Target, swc:hasAttribute, psj2:fired)			% canon has been fired
        ])
    ])
]).

% ------------------------------------------
% GatherBottles
% ------------------------------------------
% Goal for gathering bottles
% PRE:	there are a pirate and a bottle
% SUC:	the pirate has the bottle

goal_schema([
	type(psj2:'GatherBottles'),
	arguments([agens(Agens), target(Target)]),
%	charge(Happiness, 2),
	preconditions([
	    condition(true, [
			fact(Agens, swc:hasRole, psj2:'Pirate'),			% agens is a pirate,
			rule(Target, owlr:typeOrSubType, psj2:'Bottle')		% target is a bottle,
		])
	]),
	success_conditions([
		condition(true, [
			fact(Target,  swc:has, Agens)	% agens has the bottle
        ])
    ])
]).

% ------------------------------------------
% BeHappy
% ------------------------------------------
% INTEREST goal (I-goal) for maintaining some degree of happiness
% This can be realized by performing ACTIVE-PURSUIT goals (A-goals)
% which success conditions contribute to happiness
% PRE:	none, this goal is always adopted and will stay adopted permanently
% SUC:	happiness >= THRESHOLD, no action has to be taken to increase happiness
% FAIL:	happiness < THRESHOLD, action has to be taken to increase happiness

%goal_schema([
%	type(psj2:'BeHappy'),
%	arguments([agens(Agens)]),
%	preconditions([]),
%	success_conditions([])
%   ])
%]).
    	
% DEBUG HELPER FUNCTIONS
% validGoal(S, P) :-
%    goal_schema(S), validate_schema(S, P).
    
%validGoalFor(Agens, S, P) :-
%    goal_schema(S), schema_agens(S, Agens), validate_schema(S, P).    
    
%validGoalFor(Agens,S,[]),goal_success_conditions(S,C),plan(Agens,C,Plan).    

% plan('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#linda', [condition(true, [fact('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#linda', 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#has', 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#vanilla_ice_1')]), condition(true, [fact('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#linda', 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#at', 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#park_1')])], Plan).