% ---------------------------------------------------------------------------------
% Copyright (C) 2008 Human Media Interaction - University of Twente
%  
% This file is part of The Virtual Storyteller.
% 
% The Virtual Storyteller is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
% 
% The Virtual Storyteller is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with The Virtual Storyteller. If not, see <http://www.gnu.org/licenses/>.
% ---------------------------------------------------------------------------------

% Agent interface to the planner. What it does is provide functionality to 
% - Make plans
% - Adapt plans
% - Retrieve information about plans

:- module(planner, [
    plan/3,                     % (?Character, +Conditions, -Plan) 	The main method for making a plan, given conditions. 
	adapt_plan/4,               % (?Character, +Conditions, +OldPlan, -Plan) 	The main method for making a plan, given conditions. 
	invalidates_plan/2,			% (+ Plan, +Conditions)				Determines whether given conditions would invalidate given plan.
	invalid_plan/1,				% (+ Plan)							Determines if given plan has become invalid in the current context.
	executableOperator/2, 		% (+ Plan, ?Operator) 				Determines which operators in the plan are now executable.
    executableImprovisation/2,	% (+ Plan, ?Operator) 				Determines which improvisations in the plan are now executable. 
    executableEvent/2,			% (+ Plan, ?Operator) 				Determines which events in the plan are now executable.
    executableInference/2,		% (+ Plan, ?Operator) 				Determines which inference operators in the plan are now executable. 
    finished_plan/1,			% (+ Plan)							Determines whether the plan is finished (i.e., no steps to execute)
	planStep/2,					% (+ Plan, ?Step)	  				True if Step is a step in the plan
    planOrdering/2, 			% (+ Plan, ?Ordering) 				Retrieves orderings for a plan
    planLink/2, 				% (+ Plan, ?Link)	  				Retrieves causal links for a plan
    planLinkFrom/2, 			% (+ Link, ?StepName) 				Retrieves the source node for a plan link
    planLinkTo/2, 				% (+ Link, ?StepName) 				Retrieves the target node for a plan link
    planLinkCond/2	 			% (+ Link, ?Condition)				Retrieves the condition of a plan link
    ]).
    
:- use_module('pop.pl').
:- use_module('continuous.pl').
:- use_module(library(semweb/rdf_db)).

:- dynamic plan/1.
:- dynamic multiPlan/1.

% Plan predicates might contain abbreviated RDF terms; expand.
:- rdf_meta
	plan(r, t, -),
	adapt_plan(r, t, +, -),
	invalidates_plan(t, t).

% Maximum number of steps in the plan
max_plandepth(8).

% Maximum cost of the plan
max_plancost(20).

% Maximum number of framing operators in the plan
max_framing_operators(20).

% The interface to making a plan, given a list of goals (in terms of facts)
% Uses the partial-order planner to choose actions that attain the goal state, where it only selects 
% actions for given character. If Character is left unbound, actions are not selected but only
% framing operators and steps etc.
% Could eventually be replaced by continuous planning version.
plan(Character, Goals, Plan) :-
    max_plandepth(MxDepth)
,	max_plancost(MxCost)
,	max_framing_operators(MxFOs)
,   pop:iterative_pop(depth, (MxDepth, MxCost, MxFOs), Character, Goals, Plan) 
.

% Provides a hook for continuous planning, in which a given plan can be adapted. The third argument, now a don't care (_), is the
% old plan that needs to be adapted. For now, just make a new plan.
adapt_plan(Character, Goals, _, Plan2) :-
	plan(Character, Goals, Plan2).

% Determines whether given conditions invalidate an (existing) plan, requiring replanning. This is true when the conditions
% negate any of the causal links coming from the start step to any step in the plan.	
invalidates_plan(Plan, ConditionList) :-
	% Find a causal link from the start step to any step
	planLink(Plan, Link)
,	Link = (Left, _, LinkCondition)
,	startStep(Left)
	% Find a ground condition that is the inverse of the condition of the causal link
,	member(CL1, ConditionList)
,	schemas:condition_member(InvalidatingCondition, CL1)
,   ground(InvalidatingCondition)	% ground(+Term) succeeds when Term contains no free variables (SWI-Prolog)
,	pop:inverse_condition(InvalidatingCondition, LinkCondition)	% Are the invalidating condition and the causal link condition each others inverse?
.

% Determines whether given plan is invalid in the current context. 
% (1) There are unsupported links.
%     A plan becomes invalid when at least one causal link from the start step to another step in the plan is no longer valid.
invalid_plan(Plan) :-
	% Find a causal link from the start step to any step
	nonvar(Plan)
,	planLink(Plan, Link)
,	Link = (Left, _, LinkCondition)
,	startStep(Left)
	% For a plan to be invalid, this causal link's condition should no longer be valid
,	\+	schemas:validate_condition(LinkCondition)
,	!
.

% (2) There might be unnecessary links.
%     A plan becomes invalid when at least one causal link that comes from a given step that is not the start step is already
%     valid in the start step (in other words, actions are no longer necessary)
invalid_plan(Plan) :-
	nonvar(Plan)
,	planLink(Plan, Link)
,	Link = (Left, _, LinkCondition)
,	\+ startStep(Left)
,	schemas:validate_condition(LinkCondition)
,	!
.

% (3) Always invalidate when the plan contains fabula conditions (to dodge issue #0000008 on Mantis)
invalid_plan(Plan) :-
	nonvar(Plan)
,	planLink(Plan, Link)
,	Link = (_, _, LinkCondition)
,	LinkCondition = condition(_, Fact)
,	Fact =.. [fabula | _]
,	!
.
    
% executableOperator for given plan is true, when Op can be bound to an operator of an action step that is not dependend on unexecuted steps.
executableOperator(Plan, Op) :-
	planStep(Plan, (Name, Op)),
	action_schema(Op),
	firstStep(Plan, (Name, Op)).    
	
% executableImprovisation for given plan is true, when Op can be bound to an improvisation step that is not dependend on unexecuted steps.	
executableImprovisation(Plan, Op) :-
	planStep(Plan, (Name, Op)),
	framing_schema(Op),
    firstStep(Plan, (Name, Op)).

% executableInference for given plan is true, when Op can be bound to an inference step that is not dependend on unexecuted steps.	
executableInference(Plan, Op) :-
	planStep(Plan, (Name, Op)),
	inference_schema(Op),
    firstStep(Plan, (Name, Op)).

    
% executableEvent for given plan is true, when Op can be bound to an event step that is not dependend on unexecuted steps.	
executableEvent(Plan, Op) :-
	planStep(Plan, (Name, Op)),
	event_schema(Op),
    firstStep(Plan, (Name, Op)).    
    
% Whether given step is a first step in the plan. That is:
% There is no step in the orderings, that is ordered before given step, with the exception of the start step.
firstStep(Plan, Step) :-
	% Each step that might be ordered before a "first step" Step, must be the start step.
	forall( 
		planOrdering(Plan, (X, Step)),
		startStep(X)).

% Step is a step in given plan.
planStep(Plan, Step) :-
	Plan = (Steps, _Orderings, _Links, _Counters)
,	member(Step, Steps).

% Ordering is an ordering in given plan.
planOrdering(Plan, Ordering) :-
	Plan = (_Steps, Orderings, _Links, _Counters)
,	member(Ordering, Orderings).

% Link is a causal link in given plan.
planLink(Plan, Link) :-
	Plan = (_Steps, _Orderings, Links, _Counters)
,	member(Link, Links).
	
% Retieve causal link in parts (from, to, condition).
planLinkFrom(((S1name, _S1op), _S2, _C), S1name).
planLinkTo((_S1, (S2name, _S2op), _C), S2name).
planLinkCond((_S1, _S2, C), C).

% Tests whether given plan is a finished plan.
finished_plan(Plan) :-
	nonvar(Plan)
	% Each step in the plan must be a start step or a finish step
,	forall( 
		planStep(Plan, Step),
		(
			startStep(Step)
		;	finishStep(Step)
		)
	)
.	
	

% --------------- EXPERIMENTAL - not used in VST -------------------

getMultiPlan(Plan) :-
	multiPlan(MultiPlan),
	member(Plan, MultiPlan).

storeMultiPlan(Depth, X, PosGoals, NegGoals) :-
	createMultiPlan(Depth, X, PosGoals, NegGoals, Plans),
	retractall(multiPlan(_X)),
	assert(multiPlan(Plans)).

stepTail((_StepHead, StepTail), StepTail).

createMultiPlan(MaxCost, X, PosGoal, NegGoal, NewPlans) :-
	idPopC(MaxCost, PosGoal, NegGoal, P),
	planCounters(P, C),
	counterCost(C, Cost),
	Cost1 is round(Cost * X),
	maxInt(MaxInt),
	bagof(Plan, pop((MaxInt, Cost1, 0), PosGoal, NegGoal, Plan), Plans),
	planFilter(Plans, Plans, NewPlans).

planFilter(_OldPlans, [], []).

planFilter(OldPlans, [Plan| Plans], NewPlans) :-
	planSteps(Plan, Steps),
	maplist(stepTail, Steps, StepTails),
	%format('?Plan has: ~n', StepTails),
	%forall(member(ST, StepTails), format(' ~p~n', ST)),
	member(OtherPlan, OldPlans),
	OtherPlan \= Plan,
	planSteps(OtherPlan, OtherSteps),
	maplist(stepTail, OtherSteps, OtherStepTails),
	%format('!Plan has: ~n'),
	%forall(member(OST, OtherStepTails), format(' ~p~n', OST)),
	forall(member(OtherStepTail, OtherStepTails), member(OtherStepTail, StepTails)),
	%format('?Plan is out!~n'),
	planFilter(OldPlans, Plans, NewPlans).

planFilter(OldPlans, [Plan| Plans], [Plan| NewPlans]) :-
	planFilter(OldPlans, Plans,  NewPlans).
	
% Retrieves the steps from the plan.
planSteps((Steps, _Orderings, _Links, _Counters), Steps).	

% Show a plan in a nice makeup
showPlan((Steps, Orderings, Links, Counters)) :-
	format('~nSteps: ~n~n'),
	forall(member((StepHead, (StepD, _StepR)), Steps), format('~p~n~n', (StepHead, StepD))),
	format('~nOrderings: ~n'),
	forall(member(Ordering, Orderings), format('(~p)  ', Ordering)),
	format('~n'),
	format('~nLinks:~n~n'),
	forall(member(Link, Links), format('~p~n', Link)),
	format('~nCounters: ~p  ', Counters).

many(Depth, PosGoal, N1, N2) :-
	maxInt(MaxInt),
	manyPlan((Depth, MaxInt, 1), PosGoal, N1, N2).

manyC(Depth, PosGoal, N1, N2) :-
	maxInt(MaxInt),
	manyPlan((MaxInt, Depth, 1), PosGoal, N1, N2).

% Testing tool to create multiple plans
manyPlan(Max, Character, PosGoal, N1, N2) :-
	bagof(Plan, pop(Max, Character, PosGoal, [], Plan), Plans),
	length(Plans, N1),
	planFilter(Plans, Plans, NewPlans),
	forall(member(NP, NewPlans), showPlan(NP)),
	length(NewPlans, N2).

manyGood(MaxCost, X, Character, PosGoal, N1, N2) :-
	max_plandepth(Depth)
,	max_framing_operators(FOs)
,	pop:iterative_pop(cost, (Depth, MaxCost, FOs), Character, PosGoal, [], P),  % swartjes: untested.
	planCounters(P, C),
	counterCost(C, Cost),
	Cost1 is round(Cost * X),
	maxInt(MaxInt),
	bagof(Plan, pop((MaxInt, Cost1, 0), Character, PosGoal, [], Plan), Plans),
	length(Plans, N1),
	planFilter(Plans, Plans, NewPlans),
	forall(member(NP, NewPlans), showPlan(NP)),
	length(NewPlans, N2).
	
planCounters((_Steps, _Orderings, _Links, Counters), Counters).	