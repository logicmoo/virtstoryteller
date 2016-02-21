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

% Continuous planner, based on the partial order planner algorithm defined in pop.pl
% Implemented after Russell & Norvig, ch. 13 ("a situated planning agent")
%
% The algorithm works in many cases, but has a big problem, namely the following.
% Throughout the planning process, operators get more and more instantiated due to the binding constraints they receive.
% For instance, Linda's plan involving an action "EatIce" with as precondition that she has ice might specify an action to 
% pickup ice_1 thats lying on the floor (yuck). Now, if Otto suddenly decides to give Linda ice_2, the algorithm cannot see that 
% this is also fine for her EatIce action, since the preconditions of that action have been instantiated to state that 
% Linda should have ice_1 (and not ice_2, nor the initial variable Ice). 
% This has to do with the fact that you cannot "undo" / "remove" bindings in the planning process, as stated in Russell & Norvig, 
% and is a difficult problem. The result is that actions will sometimes stay in the plan until the goal happens to be achieved.
% 
% --- Solution 1. Instead of using member/2 in pop.pl's effectMember/2, use unifiable/3. Example:
%		Precond = [weather(A), weather(B)], member(X, Precond), unifiable(X, weather(sunny), Bindings)
%	yields amongst other solutions:
% 		Precond = [weather(A), weather(B)],
%		X = weather(A),
%		Bindings = [A=sunny]
%	Using this solution, the planner can keep track of the bindings made in the planning process in a list. It can associate the bindings
%	with the causal links that produced them, so that if causal links are thrown away, the bindings are removed from the list 
%	(possible danger: what if two causal links have produced the same bindings? - do not keep a general list but only wrt causal link?).
%   Then later on, upon retrieval of the steps and the causal links for execution, we can substitute the variables for their values:
%		bind(Bindings)   ( where bind/1 is implemented by recursing over the list of bindings 
%						   and using bind([Var=Val|Tail]) :- Var=Val, bind(Tail).
%
% --- Solution 2.
% 	Using attributed variables, we can explicitly store the variables and values used in the plan. This is a bit of a complicated solution though:
%	See swi-prolog mailing list, 23-7-2008
%
% --- Solution 3.
%	Before doing member/2 in effectMember/2, copy the term (using copy_term/2) so that the original condition is preserved.
%
% NOTE: pay attention to the framing operator; it has extra causal links to the start step, are they properly managed too?

:- module(continuous, [
	c_plan/3	% Continuous planning, given character, goals and (initial) plan
,	c_plan/4	% Continuous planning, without initial plan.
]).

:- use_module('pop.pl').
:- use_module('schema_management.pl').
:- use_module(library(debug)).

plandepth(8).

% Variant for no plan given: just construct a plan
c_plan(Character, Goals, Plan) :-
	var(Plan)
,   plandepth(Depth)
,   pop:iterative_pop(depth, (Depth, 20, 2), Character, Goals, Plan) 
.

% Variant for plan given: repair plan
c_plan(Character, _, Plan, NewPlan) :-
	nonvar(Plan)
,	Plan = (_,_,_,Counters)
,	plandepth(Depth)
,	pop:iterator(depth, Counters, (Depth, 20, 2), Character, Plan, NewPlan1)
,	ignore((
		debugging(plan_control)
	,	prolog_debug:debug(plan_control, 'Starting to clean plan...',[])
	))
,	remove_unsupported_links(NewPlan1, NewPlan2)
,	extend_causal_links(NewPlan2, NewPlan3)
,	remove_redundant_operators(NewPlan3, NewPlan)
.

% Remove links that are no longer supported by the start state
remove_unsupported_links(Plan1, Plan2) :-
	Plan1 = (Steps, Orderings, Links1, Counters)
,	startStep(S1) % The startstep...
,	member((S1,S2,Condition), Links1)	% ...occurs in a causal link
,	\+ schemas:validate_condition(Condition) % ...but the link is no longer valid
,	!	% Cut, we have found an unsupported link; remove & recurse
,	ignore((
		debugging(plan_control)
	,	prolog_debug:debug(plan_control, 'Removing unsupported links...',[])
	))
,	delete(Links1, (S1, S2, Condition), Links2)
%	Here, the associated bindings should also be deleted. No idea how to do this if we have not explicitly tracked them.
%   ...
,	remove_unsupported_links((Steps, Orderings, Links2, Counters), Plan2)
.

% If there are no unsupported links, leave plan intact.
remove_unsupported_links(Plan, Plan).

% Extend causal links back to start step
extend_causal_links(Plan1, Plan2) :-
	Plan1 = (Steps, Orderings, Links1, Counters)
,	member((S1, S2, Condition), Links1)	% A causal link...
,	\+ startStep(S1)	% Not already the start step
,	startStep(S3)	% Take the start step
,	schemas:validate_condition(Condition)	% See if it satisfies the link
,	\+ pop:threatens(Orderings, _, (S3, S2, Condition)) % ...the link (S3, S2, Condition) is safe...
,	! % Cut, we have found an extendable causal link; extend & recurse
,	ignore((
		debugging(plan_control)
	,	prolog_debug:debug(plan_control, 'Extending causal links to start step...',[])
	))
,	delete(Links1, (S1, S2, Condition), Links2) % (delete old link)
,	extend_causal_links((Steps, Orderings, [(S3, S2, Condition) | Links2], Counters), Plan2)
.

% Extend causal links back to earliest possible step
extend_causal_links(Plan1, Plan2) :-
	Plan1 = (Steps, Orderings, Links1, Counters)
,	member((S1, S2, Condition), Links1)	% A causal link...
,	member(S3, Steps) % ...there is another step...
,	pop:ordered(S3, S1, Orderings) % ...which is ordered before S1...
,	pop:effectMember(Condition, S2) % ...and it satisfies the condition of the step
,	\+ pop:threatens(Orderings, _, (S3, S2, Condition)) % ...the link (S3, S2, Condition) is safe...
	% TODO: "safe" means also that it is correct (i.e. the new step must _establish_ the causal link too
	% TODO: start step is special case, query instead of effectmember
,	! % Cut, we have found an extendable causal link; extend & recurse
,	ignore((
		debugging(plan_control)
	,	prolog_debug:debug(plan_control, 'Extending causal links...',[])
	))
,	delete(Links1, (S1, S2, Condition), Links2) % (delete old link)
,	extend_causal_links((Steps, Orderings, [(S3, S2, Condition) | Links2], Counters), Plan2)
.

% If there are no causal links to extend, leave plan intact.
extend_causal_links(Plan, Plan).

% Remove redundant operators from plan
remove_redundant_operators(Plan1, Plan2) :-
	Plan1 = (Steps1, Orderings, Links, Counters)
,	member(S, Steps1)	% There is a step
,	\+ member((S, _, _), Links)	% That supplies no links
,	\+ finishStep(S)	% But is not the finish step
,	! % Cut, we have found a redundant operator; remove & recurse
,	ignore((
		debugging(plan_control)
	,	prolog_debug:debug(plan_control, 'Removing redundant operators...',[])
	))
,	delete(Steps1, S, Steps2)
,	S = (_, Op)
,	operator_duration(Op, D)
,	Counters = (Depth, Cost, Improvs)
,	NewDepth is Depth - 1
,	NewCost is Cost - D
,	(framing_schema(Op) -> NewImprovs is Improvs - 1 ; NewImprovs is Improvs)
% hmmz, wat doet deze regel? :S
,	(inference_schema(Op) -> NewImprovs is Improvs - 1 ; NewImprovs is Improvs)
,	delete_redundant_links(S, Links, Links2)
,	delete_redundant_orderings(S, Orderings, Orderings2)
,	remove_redundant_operators((Steps2, Orderings2, Links2, (NewDepth, NewCost, NewImprovs)), Plan2)   
.

% If there are no redundant operators, leave plan intact.
remove_redundant_operators(Plan, Plan).

% Delete causal links that are redundant for given step (the incoming links)
% (outgoing links should already be gone, and if not, will be deleted by other choice points)
delete_redundant_links(RemovedStep, Links, Links2) :-
	member((Sprev, RemovedStep, Cond), Links)
,	!
, 	delete(Links, (Sprev, RemovedStep, Cond), Links_inter)
,	delete_redundant_links(RemovedStep, Links_inter, Links2)
.

% If there are no redundant links, leave links intact
delete_redundant_links(_, Links, Links).

% Delete orderings that are redundant for given step  (the incoming orderings)
% (outgoing orderings should already be gone, and if not, will be deleted by other choice points)
delete_redundant_orderings(RemovedStep, Orderings, Orderings2) :-
	member((Sprev, RemovedStep), Orderings)
,	!
, 	delete(Orderings, (Sprev, RemovedStep), Orderings_inter)
,	delete_redundant_orderings(RemovedStep, Orderings_inter, Orderings2)
.

% Delete orderings that are redundant for given step  (the outgoing orderings)
delete_redundant_orderings(RemovedStep, Orderings, Orderings2) :-
	member((RemovedStep, Snext), Orderings)
,	!
, 	delete(Orderings, (RemovedStep, Snext), Orderings_inter)
,	delete_redundant_orderings(RemovedStep, Orderings_inter, Orderings2)
.

% If there are no redundant orderings, leave orderings intact
delete_redundant_orderings(_, Orderings, Orderings).
