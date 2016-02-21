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

% Partial Order planner
% @author Kruizinga
% Modified by Ivo Swartjes
% 22-03-2007 - 15-06-2007 - 1-10-2007 - 7-12-2007
%
% A plan is a tuple (Steps, Orderings, Links, Counters)
% 	Steps is a list of tuples (StepName, StepOperator)
% 	Orderings is a list of tuples (BeforeStep, AfterStep)
% 	Links is a list of tuples (BeforeStep, AfterStep, CausalLink)
% 	Counters is a triple (Depth, Cost, Improvs)

:- module(pop, [
    iterative_pop/5,        % +iterator type, +max, +character, +goal, -Plan
    iterative_improv_pop/5, % +iterator type, +max, +character, +goal, -Plan
    pop/4,                  % +max, +character, +goal, -Plan
    startStep/1,            % ?Step
    finishStep/1            % ?Step
    ]).
    
:- use_module('knowledgebase.pl').
:- use_module('schema_management.pl').
:- use_module(library(debug)).

% maximum number to use for counters
%maxInt(1000).

% Debug options. Put options to true if you want to see debug information (e.g. found plan threats, intermediate plans that are being made etc.)
:- prolog_debug:debug(plan_control, 		false).
:- prolog_debug:debug(flaws,		 		false).
:- prolog_debug:debug(threats, 				false).
:- prolog_debug:debug(operator_selection, 	false).
:- prolog_debug:debug(intermediate_plans, 	false).

% Selectors for depth, cost, and number of framing operators.
counterDepth((Depth, _Cost, _Improvs), Depth).
counterCost((_Depth, Cost, _Improvs), Cost).
counterImprovs((_Depth, _Cost, Improvs), Improvs).

counterZero((0,0,0)).

% Anything of below form (i.e., any finished step with some preconditions filled in) 
%    is a schema. CAREFUL: somehow this means we can make a plan for any goal by instantiating this predicate smartly.
schemas:schema([	
	class(pop)
,	type(finished)
,	preconditions(_)])
.	
	
% ----------------------------------
% Plan control (Iterative Deepening)
% ----------------------------------


% Iterative Deepening version of the partial-order planner. 
%	 Iterative deepening provides speedup and control over the planner: according to some definition of "depth", the algorithm tries
%	 to find a plan with a maximum depth of N (starting at 0), and if it cannot find it, increases the maximum depth to N+1 and tries
%	 to find a solution again. Most easy to understand is the depth iterator:
%		try to find a plan with 0 steps in it (almost always fails)
%		try to find a plan with 1 steps in it
%		try to find a plan with 2 steps in it
%		etc. until we reach the maximum depth specified in Max; then the planner fails. 
% Type indicates what to iterate over:
%    depth: iterate over number of steps in the plan
%    cost: iterate over maximum cost of the plan
iterative_pop(Type, Max, Character, Goal, Plan) :-    
    makeMinimalPlan(Goal, MinimalPlan)
, 	!
,	iterator(Type, 0, Max, Character, MinimalPlan, Plan).
    

% Iterative Deepening version of the partial-order planner for improvisations. 
% uses the Iterative Deepening version of partial-order planning, but also iterates over the number of used improvisations.
% Type indicates what to iterate over (besides the number of improvisations):
%    depth: iterate over number of steps in the plan
%    cost: iterate over maximum cost of the plan    
iterative_improv_pop(Type, Max, Character, Goal, Plan) :-
    makeMinimalPlan(Goal, MinimalPlan)
, 	!	
,	improv_iterator(Type, 0, Max, Character, MinimalPlan, Plan).
    
    
% iterator over depth
iterator(depth, Current, (D, C, I), Character, OldPlan, Plan) :-
	findPlan((Current, C, I), Character, OldPlan, Plan)
  	 % The cut prevents the iterator from increasing the depth even though a plan has been found. But this excludes alternative (longer) plans.
  	 % This is a tradeoff to be made.
  	 % A problem with not using the cut is that longer plans might (again) be the minimal plan. For instance, if maxDepth is 5, this predicate
  	 % will present 5 alternatives that are all the minimal plan.
,   !
;	(		Current1 is Current + 1 
		,	Current1 =< D
		,	prolog_debug:debug(plan_control, 'Increasing depth to ~p~n', Current1)
		,	iterator(depth, Current1, (D, C, I), Character, OldPlan, Plan)
	)
	.
	

% iterator over cost
iterator(cost, Current, (D, C, I), Character, OldPlan, Plan) :-
	findPlan((D, Current, I), Character, OldPlan, Plan)
  	 % The cut below prevents the iterator from increasing the depth even though a plan has been found. But this excludes alternative (longer) plans.
  	 % This is a tradeoff to be made.	
  	 % A problem with not using the cut is that longer plans might (again) be the minimal plan. For instance, if maxCost is 5, this predicate
  	 % will present 5 alternatives that are all the minimal plan.  	 
,   !
;	(		Current1 is Current + 1
		,	Current1 =< C
		,	prolog_debug:debug(plan_control, 'Increasing cost to ~p~n', Current1)
		,	iterator(cost, Current1, (D, C, I), Character, OldPlan, Plan)
	)
.
	
	
% iterate over improvisations as well as depth or cost, set Type to either of these
improv_iterator(Type,  Current, (D, C, I), Character, OldPlan, Plan) :-
	iterator(Type, 0, (D, C, Current), Character, OldPlan, Plan)
  	 % A cut (!) here would prevent the iterator from increasing the depth even though a plan has been found. 
  	 % But this excludes alternative (longer) plans.
  	 % That's why there's currently no cut.
;
	(		Current1 is Current + 1
		,	Current1 =< I
		,	prolog_debug:debug(plan_control, 'Increasing improvs to ~p~n', Current1)
		,	improv_iterator(Type, Current1, (D, C, I), Character, OldPlan, Plan)
	)
.



% ----------------------------------
% POP planning algorithm
% ----------------------------------

% pop/5 +Max, ?Character +PosGoal, + NegGoal, ?Plan
% pop returns a partial order plan for given character
% uses the database as input
pop(Max, Character, Goal, Plan) :-
	makeMinimalPlan(Goal, MinimalPlan)
,	!	% NOTE: cut, because there is no decision in makeMinimalPlan that can influence correct goal choice. True?
,	findPlan(Max, Character, MinimalPlan, Plan)
.
	

% startStep defines the start step of a plan.
startStep((s, dummy))
.

% finishStep defines the finish step of a plan.
finishStep((f, _))
.


% makeMinimalPlan/2 +Goal, ?Plan
% makeMinimalPlan returns an initial plan containing a start and a finish step, and depth/cost/improvs at 0.
% this is a plan for given character.
% The finish step is a helper step, containing the goals as preconditions
makeMinimalPlan(Goal, ([FinishStep], [(StartStep, FinishStep)], [], CounterZero)) :-
	counterZero(CounterZero)
,	startStep(StartStep)
,	FinishStep = (f, [
						class(pop),
						type(finished),
						preconditions(Goal)
				])
,	FinishStep = (_Name, FinishSchema)
	% For FinishSchema, the predicate schema(FinishSchema) now holds, according to the schema definition in schema.pl
	% Check anyway, to make sure:
,	schema(FinishSchema)
.		
	

% Find a plan, based on given (possibly partial) plan.
% If a precondition is chosen, findPlan commits to solve it (hence the cut). This is because
% in the end, EVERY precondition must be solved, so it makes no sense to consider other choices
% of preconditions when findPlan for a particular chosen precondition fails. Therefore, findPlan should not 
% leave choice points for the choice of precondition.
findPlan(Max, Character, Plan, NewPlan) :-
	% IF we can choose an open precondition,
	prolog_debug:debug(flaws, '-------- New cycle --------',[])
,	ignore((
		debugging(intermediate_plans)
	,	show_plan(Plan)
	))
,	prolog_debug:debug(flaws, 'Choosing open precondition...', [])	
,	choosePrecondition(Plan, Step, Condition)	% choosePrecondition prunes its own choice points (using CUT)
,	ignore((
		debugging(flaws)
%	,	show_condition(Condition, CondTxt)
	,	show_step(Step, Txt)
	,	prolog_debug:debug(flaws, 'FLAW: Working on precondition: ~w~nof step ~w',[Condition, Txt])
	))

    % THEN choose an operator for this condition, resolve the threats that arise, and recursively call findPlan again.
,	chooseOperator(Max, Character, Plan, Step, Condition, Plan2)
,	resolveThreats(Plan2, Plan3)
,	findPlan(Max, Character, Plan3, NewPlan)
.

% Variant where there are no open preconditions anymore; leave the plan intact.
% There are also no causal threats anymore, since they were resolved in the last iteration of the other findPlan variant.
% NOTE: this is the place to choose a binding for unbound variables in steps of the plan if we allow them
%		(e.g., goal: have painting experience, plan: paint _something_)
findPlan(_, _, Plan, Plan) :- 
	\+ choosePrecondition(Plan, _, _)	% we cannot choose a precondition anymore
,	prolog_debug:debug(flaws, 'No more open preconditions.', [])
. 


% Choose precondition -- of the form   condition(Truth, Fact).
%
% Note: although the choice is nondeterministic, the execution works as LIFO (last in first out), choosing most recent open preconditions first. 
%   This is the result of adding new steps to the front of the steps list. The POP literature has argued for LIFO to be a good strategy, because 
%   it forces the planner to "focus on a particular (sub)goal in a depth-first manner". 
choosePrecondition((Steps, _Orderings, Links, _Depth), (Stepname, StepOperator), SubCondition) :-
	prolog_debug:debug(flaws, '# Choose precondition', [])
	% Choose a step from the plan (choice point)
,	member((Stepname, StepOperator), Steps)
	% Retrieve its preconditions
,	schema_preconditions(StepOperator, Preconditions)
	% Select one (choice point)
,   member(Condition, Preconditions)
	% Which is not already satisfied
,	unsatisfied(Condition, (Stepname, StepOperator), Links)
	% Cut, commit to resolving this condition. If we cannot, we also cannot find a plan.
	% Note: this commitment drastically speeds up the search process!
,	!
	% Select an open subcondition for the condition (possible choice point)
%,	prolog_debug:debug(flaws, 'Finding open subcondition...~n', [])
,	choose_open_subcondition(Condition, SubCondition, (Stepname, StepOperator), Links)
,	prolog_debug:debug(flaws, 'Found open subcondition:~n~w~n', [SubCondition])
.	

% Choose sub-precondition member -- of the form   condition(true, Fact).
choose_open_subcondition(Condition, SubCondition, Step, Links) :-
	Condition = condition(true, _)
,	schemas:condition_member(SubCondition, Condition)
,	\+ achieved_subcondition(SubCondition, Step, _, Links)
, 	!   % CUT, leave no choice points for choosing true preconditions. If we cannot find an operator for this condition, 
		% no plan is possible; findPlan fails on this depth and the thing backtracks.
.
	
% Choose precondition member -- of the form   condition(false, Fact).
choose_open_subcondition(Condition, SubCondition, Step, Links) :-	
	Condition = condition(false, _)
,	schemas:condition_member(SubCondition, Condition)
,	achieved_subcondition(SubCondition, Step, _, Links)
,	!
,	fail	% Cut-and-fail: if we can find a member that is achieved, there is no open subcondition anymore, do not try alternatives.
.	


% Choose precondition member -- of the form   condition(false, Fact).
% only when there is not an already achieved subcondition as checked by predicate above.
choose_open_subcondition(Condition, SubCondition, Step, Links) :-	
	Condition = condition(false, _)
,	schemas:condition_member(SubCondition, Condition)
,	\+ achieved_subcondition(SubCondition, Step, _, Links)
%,	prolog_debug:debug(flaws, 'Negative precondition not achieved', [])
.	% There's a choice point here (NO cut). Any one of the negative preconditions must be achieved.


% Checks whether given condition of step Step is archieved by AchievingStep, considering the causal links of the plan.
% Note:    no need for a definition of  achieved(Condition, Step, []), because this fails. 
% Warning: the semantics of this predicate are incorrect because different conditions that can be unified with the same link will 
%		   be determined to be "achieved" even though only one is; this becomes a problem when e.g. later conditions specify that 
%		   the variables should have different values. E.g.,
%				fact(O1, rdf:type, swc:'Object')
%				fact(O2, rdf:type, swc:'Object')
%				rule(O1, owlr:isNot, O2)
%		   in which a link saying (ball, rdf:type, swc:'Object') unifies with both the first fact and the second fact, becoming a 
%		   problem when the rule is encountered.
achieved_subcondition(Cond, Step, AchievingStep, Links ) :-
	Link = (AchievingStep, Step, Cond)  % Unify link with a link between given step and an achieving step.
,	memberchk(Link, Links)
.

% A condition if given step is unsatisfied, if we can choose an open subcondition.
unsatisfied(Condition, Step, Links) :-
	choose_open_subcondition(Condition, _, Step, Links)
.

%,	Step2 \== AchievingStep
%,	PP = PP2
%,	NP = NP2
%,	!
%,	show_step(AchievingStep, AchievingStepT)
%,	show_step(Step, StepT)
%,	show_condition(Cond, CondTxt)
%,	prolog_debug:debug(plan_control, 'achieved!~n~w of ~w~n achieved by ~w~n', [CondTxt, StepT, AchievingStepT])

% Checks whether given condition is archieved by given step, considering the causal links of the plan.
%achieved_subcondition(Condition, Step, AchievingStep, [_ | Links]) :-
%	achieved_subcondition(Condition, Step, AchievingStep, Links).
	
	

% chooseOperator/7 +Max, ?Character, +CurrentPlan, +Step, +PosCondition, +NegCondition, -BetterPlan
% Chooses an operator to resolve a precondition for a given step.
% NOTE: add cut after chooseStart? Because if a goal is already true, we don't want to use other actions in order to make them true. 
%       * without cut, the behaviour is something like, if I can use chooseStart, but can't find a plan after using it, forget it and try chooseStep anyway
%		* with cut, it is something like, if I can use chooseStart, but when I've done so (in all possible ways), and I can't find a plan for any of them, then there is no plan.
%        (would a cut exclude "go to forest, pick up treasure, come back to house" for a goal "be in house with treasure"?)
chooseOperator(Max, _, CurrentPlan, (AskStepname, AskStepoperator), Condition, NewPlan) :-
	prolog_debug:debug(flaws, '# Choose operator', [])

	% choose the start state, an existing step from the plan, or a new operator	
,   chooseStart(Max, CurrentPlan, (AskStepname, AskStepoperator), Condition, NewPlan)
.

% Choose an operator for conditions (unless they are of the type "rule(_,_,_)" -> todo: replace by is_plannable(...) or something.
chooseOperator(Max, Character, CurrentPlan, (AskStepname, AskStepoperator), Condition, NewPlan) :-
	is_plannable(Condition)		% For speedup, only consider "plannable" conditions.
,	chooseStep(CurrentPlan, (AskStepname, AskStepoperator), Condition, NewPlan)
	% In-Character (IC):
;	chooseAction(Max, Character, CurrentPlan, (AskStepname, AskStepoperator), Condition, NewPlan)
;	chooseExpectation(Max, Character, CurrentPlan, (AskStepname, AskStepoperator), Condition, NewPlan)
	% Out-Of-Character (OOC):
;	chooseEvent(Max, CurrentPlan, (AskStepname, AskStepoperator), Condition, NewPlan)
;	chooseFraming(Max, CurrentPlan, (AskStepname, AskStepoperator), Condition, NewPlan)
;	chooseInference(Max, CurrentPlan, (AskStepname, AskStepoperator), Condition, NewPlan)
.

% Checks whether given condition is "plannable", i.e., we can directly select operators to achieve it.
is_plannable(Condition) :-
	Condition \= rule(_,_,_).
	
	
% Calculate condition cost.
conditionCost(fact(_S, R, O), D) :-	
	R = 'http://www.owl-ontologies.com/StoryWorldCore.owl#length'
,	O = literal(type('http://www.w3.org/2001/XMLSchema#int', L))
,	!
,	atom_to_term(L, D, _Bindings)
,	prolog_debug:debug(operator_selection, '      Cost of relation: ~p~n',D)
.

% Condition cost is zero if it doesn't match above predicate.
conditionCost(_, 0).
	

% chooseStart
% reuse the start (current) state
chooseStart(Max, (Steps, Orderings, Links, (Depth, Cost, Improvs)), AskStep, Condition, NewPlan) :-
	% Unification by just querying the KB whether the condition is true. (also works like this in FAtiMA)
	% As opposed to standard POP where the start step contains the conditions as effects.
	schemas:validate_condition(Condition)
,	conditionCost(Condition, D)
,	counterCost(Max, MaxCost)
,	Cost1 is Cost + D
,	Cost1 =< MaxCost
,	startStep(StartStep)
,	add_to_set((StartStep,AskStep), Orderings, Orderings2)
,	add_to_set((StartStep, AskStep, Condition), Links, Links2)
,	NewPlan = (Steps, Orderings2, Links2, (Depth, Cost1, Improvs))
,	ignore((
		debugging(operator_selection)
	,	show_condition(Condition, Txt)
	,	prolog_debug:debug(operator_selection, '   # choosing start step: ~w~n',[Txt])
	))
.
	
% chooseStep
% reuse a step in the plan that achieves the precondition
chooseStep((Steps, Orderings, Links, Counters), AskStep, Condition, NewPlan) :-
	member(Step, Steps)
,	possible_after(Orderings, (AskStep, Step))
,	Step = (_, Op)
,	effectMember(Condition, Op)
,	add_to_set((Step,AskStep), Orderings, Orderings2)
,	add_to_set((Step, AskStep, Condition), Links, Links2)
,	NewPlan = (Steps, Orderings2, Links2, Counters)
,	ignore((
		debugging(operator_selection)
	,	show_step(Step, Txt)
	,	prolog_debug:debug(operator_selection, '   # choosing existing step: ~w~n', Txt)
	))
.


% chooseAction
% put an action operator in the plan that achieves the precondition
chooseAction(Max, Character, (Steps, Orderings, Links, (Depth, Cost, Improvs)), AskStep, Condition, NewPlan) :-
	AskStep = (_, AskStepOperator)
,	schemas:in_character(AskStepOperator)	% Cannot choose action to resolve any out of character schemas
%,	\+ framing_schema(AskStepOperator)	% Cannot choose action to resolve framing schema
%,	\+ event_schema(AskStepOperator)	% Cannot choose action to resolve event schema
,	nonvar(Character)					% Cannot choose action if character is not given
,	counterDepth(Max, MaxDepth)
,	counterCost(Max, MaxCost)
,	Depth1 is Depth + 1
,	Depth1 =< MaxDepth
,	action_schema(NewOperator)
,	schema_agens(NewOperator, Character) % Set agens of chosen schema to given character
,	effectMember(Condition, NewOperator)
,	operator_duration(NewOperator, D)
,	Cost1 is Cost + D
,	Cost1 =< MaxCost
,	gensym(step, Name)
,	NewStep = (Name, NewOperator) % Depth is used as the name for a new operator
,	add_to_set((NewStep,AskStep), Orderings, Orderings2)
,	add_to_set((NewStep, AskStep, Condition), Links, Links2)
,	NewPlan = ([NewStep| Steps], Orderings2, Links2, (Depth1, Cost1, Improvs))
,	ignore((
		debugging(operator_selection)
	,	show_step(AskStep, TxtAsk)
	,	show_step(NewStep, Txt)
	,	show_condition(Condition, CondTxt)
	,	prolog_debug:debug(operator_selection, '   # choosing NEW action: ~w', Txt)
	,	prolog_debug:debug(operator_selection, '     because it achieves: ~w', CondTxt)
	,	prolog_debug:debug(operator_selection, '     that is			: ~w', Condition)
	,	prolog_debug:debug(operator_selection, '     of step            : ~w', TxtAsk)
	))
.

% chooseExpectation
% put an expectation schema in the plan that achieves the precondition
chooseExpectation(Max, Character, (Steps, Orderings, Links, (Depth, Cost, Improvs)), AskStep, Condition, NewPlan) :-
	AskStep = (_, AskStepOperator)
,	schemas:in_character(AskStepOperator)
%,	\+ framing_schema(AskStepOperator)	% Cannot choose expectation to resolve framing schema
%,	\+ event_schema(AskStepOperator)	% Cannot choose expectation to resolve event schema
,	nonvar(Character)					% Cannot choose expectation if character is not given
,	counterDepth(Max, MaxDepth)
,	counterCost(Max, MaxCost)
,	Depth1 is Depth + 1
,	Depth1 =< MaxDepth
,	expectation_schema(NewOperator)
%,	schema_agens(NewOperator, Character) % Set agens of chosen schema to given character (WHY?)
,	effectMember(Condition, NewOperator) % See if chosen schema is applicable to precondition to search for
,	Cost1 is Cost + 1
,	Cost1 =< MaxCost
,	gensym(step, Name)
,	NewStep = (Name, NewOperator) % Make new step
,	add_to_set((NewStep,AskStep), Orderings, Orderings2) % Add orderings to plan
,	add_to_set((NewStep, AskStep, Condition), Links, Links2) % Add links to plan
,	NewPlan = ([NewStep| Steps], Orderings2, Links2, (Depth1, Cost1, Improvs))
,	ignore((
		debugging(operator_selection)
	,	show_step(AskStep, TxtAsk)
	,	show_step(NewStep, Txt)
	,	show_condition(Condition, CondTxt)
	,	prolog_debug:debug(operator_selection, '   # choosing NEW expectation: ~w', Txt)
	,	prolog_debug:debug(operator_selection, '     because it achieves: ~w', CondTxt)
	,	prolog_debug:debug(operator_selection, '     that is			: ~w', Condition)
	,	prolog_debug:debug(operator_selection, '     of step            : ~w', TxtAsk)
	))
.

% chooseImprovisation
% put an improvisation operator in the plan that achieves the precondition
chooseEvent(Max, (Steps, Orderings, Links, (Depth, Cost, Improvs)), AskStep, Condition, NewPlan) :-
	counterDepth(Max, MaxDepth)
,	counterCost(Max, MaxCost)
,	Depth1 is Depth + 1
,	Depth1 =< MaxDepth
,	Cost1 is Cost + 1
,	Cost1 =< MaxCost
,	event_schema(NewOperator)
,	effectMember(Condition, NewOperator)
	%   Code commented out below is not necessary: endowment allows one to improvise for other characters as well.
	%	getFromSchema(NewOperator, head(H)),
	%	getFromHead(H, agens(Agens)),
	%	query((myself, iam, Agens)),
	%	checkAgent(NewStep),
,	gensym(step, Name) % Generates unique symbol, e.g. "step342"
,	NewStep = (Name, NewOperator)
,	add_to_set((NewStep,AskStep), Orderings, Orderings2)
,	add_to_set((NewStep, AskStep, Condition), Links, Links2)
% Also add causal links from the framing operator to the start step to ensure that no actions executed earlier will contradict the effects
% of the framing operator, for which the illusion should be cast that these effects were always true (see paper).
,	NewPlan = ([NewStep| Steps], Orderings2, Links2, (Depth1, Cost1, Improvs))
	%%show_step(NewStep, Txt),
	%%prolog_debug:debug(operator_selection, '   # choosing NEW improvisation: ~w~n', Txt)
	
		%,format('New improvisation new link:~n~p~n~n', (Depth, AskStepname, PosCondition, NegCondition))
.
	
% chooseSubgoal
% put a subgoal in the plan that achieves the precondition, to be seen as a type of abstract operator in terms of DPOCL, 
% but with no predefined decomposition
% Ignore failure conditions for now.
chooseSubgoal(Max, Character, (Steps, Orderings, Links, (Depth, Cost, Improvs)), AskStep, Condition, NewPlan) :-
	counterDepth(Max, MaxDepth)
,	counterCost(Max, MaxCost)
,	Depth1 is Depth + 1
,	Depth1 =< MaxDepth
,	goal_schema(NewOperator)
,	schema_agens(NewOperator, Character) % Set agens of chosen schema to given character
,	gensym(step, Name)  % Generates unique symbol, e.g. "step342"
,	NewStep = (Name, NewOperator)
,	\+ memberchk((_D, NewOperator), Steps)  % Shouldnt already have this goal in my abstract plan.
,	effectMember(Condition, NewOperator)
,	Cost1 is Cost + 1
,	Cost1 =< MaxCost
,	add_to_set((NewStep,AskStep), Orderings, Orderings2)
,	add_to_set((NewStep, AskStep, Condition), Links, Links2)
,	NewPlan = ([NewStep| Steps], Orderings2, Links2, (Depth1, Cost1, Improvs))
.	

chooseInference(Max, (Steps, Orderings, Links, (Depth, Cost, Improvs)), AskStep, Condition, NewPlan) :-
	counterDepth(Max, MaxDepth)
,	counterImprovs(Max, MaxImprovs)
,	counterCost(Max, MaxCost)
,	Depth1 is Depth + 1
,	Depth1 =< MaxDepth
,	Improvs1 is Improvs + 1
,	Improvs1 =< MaxImprovs
,	Cost1 is Cost + 1
,	Cost1 =< MaxCost
,	inference_schema(NewOperator)
,	effectMember(Condition, NewOperator)
	%   Code commented out below is not necessary: endowment allows one to improvise for other characters as well.
	%	getFromSchema(NewOperator, head(H)),
	%	getFromHead(H, agens(Agens)),
	%	query((myself, iam, Agens)),
	%	checkAgent(NewStep),
,	gensym(step, Name) % Generates unique symbol, e.g. "step342"
,	NewStep = (Name, NewOperator)
,	add_to_set((NewStep,AskStep), Orderings, Orderings2)
,	add_to_set((NewStep, AskStep, Condition), Links, Links2)
,	NewPlan = ([NewStep| Steps], Orderings2, Links2, (Depth1, Cost1, Improvs1))
.

% chooseImprovisation
% put an improvisation operator in the plan that achieves the precondition
chooseFraming(Max, (Steps, Orderings, Links, (Depth, Cost, Improvs)), AskStep, Condition, NewPlan) :-
	counterDepth(Max, MaxDepth)
,	counterImprovs(Max, MaxImprovs)
,	counterCost(Max, MaxCost)
,	Depth1 is Depth + 1
,	Depth1 =< MaxDepth
,	Improvs1 is Improvs + 1
,	Improvs1 =< MaxImprovs
,	Cost1 is Cost + 1
,	Cost1 =< MaxCost
,	framing_schema(NewOperator)
,	effectMember(Condition, NewOperator)
	%   Code commented out below is not necessary: endowment allows one to improvise for other characters as well.
	%	getFromSchema(NewOperator, head(H)),
	%	getFromHead(H, agens(Agens)),
	%	query((myself, iam, Agens)),
	%	checkAgent(NewStep),
,	gensym(step, Name) % Generates unique symbol, e.g. "step342"
,	NewStep = (Name, NewOperator)
,	add_to_set((NewStep,AskStep), Orderings, Orderings2)
,	add_to_set((NewStep, AskStep, Condition), Links, Links2)
% Also add causal links from the framing operator to the start step to ensure that no actions executed earlier will contradict the effects
% of the framing operator, for which the illusion should be cast that these effects were always true (see paper).
,	add_framing_effects_as_causal_links(NewStep, Links2, Links3)
,	NewPlan = ([NewStep| Steps], Orderings2, Links3, (Depth1, Cost1, Improvs1))
	%%show_step(NewStep, Txt),
	%%prolog_debug:debug(operator_selection, '   # choosing NEW improvisation: ~w~n', Txt)
	
		%,format('New improvisation new link:~n~p~n~n', (Depth, AskStepname, PosCondition, NegCondition))
.

add_framing_effects_as_causal_links(FramingStep, LinksIn, LinksOut) :-
	startStep(StartStep)
,   FramingStep = (_, FramingOperator)
,	effectMember(FramingEffect, FramingOperator)
,	\+ member((StartStep, FramingStep, FramingEffect), LinksIn)	
,	!
,	append(LinksIn, [(StartStep, FramingStep, FramingEffect)], LinksIn2)	
,	add_framing_effects_as_causal_links(FramingStep, LinksIn2, LinksOut)		
.

add_framing_effects_as_causal_links(_, LinksIn, LinksIn).

effectMember(Condition, Operator) :-
	operator_effects(Operator, E),
	member(E1,E),
	schemas:condition_member(E2,E1)

	% Try unifying the condition with a member of the list for the operator, in all possible ways (hence no memberchk)
,	Condition = E2

	% TODO (but think about first): a potential speedup is achieved by checking if the newly chosen operator does not introduce 
	% open precondition flaws that already existed. I'm sure one can prove easily that under certain conditions, a plan with 
	% this property should be pruned. 
	% -> open precondition flaw must exist in the "branch" that the planner is currently considering!
.

	

resolveThreats((Steps, Orderings, Links, Counters), BetterPlan) :-
	prolog_debug:debug(flaws, '# Resolve threats', [])
	
	% IF we can find a threat, 
,	(
		member(Step, Steps)
	,	\+ finishStep(Step)
	,	member(Link, Links)
	,	threatens(Orderings, Step, Link)
	) 
-> 
	% THEN apply a repair strategy and look again for threats
	(
		% one strategy for resolving threats: order the step either before or after the step that it threatens
		prolog_debug:debug(threats, 'Resolving threat...',[])
	,	makeOrdering(Orderings, Step, Link, Ordering)
%	,	prolog_debug:debug(threats, 'Made ordering...~n',[])
	,	Ordering = (StepLeft, StepRight)
	,	ignore((
			debugging(threats)
		,	show_step(StepLeft, LTxt)
		, 	show_step(StepRight, RTxt)
		,	prolog_debug:debug(threats, 'NEW ORDERING: placing ~w before ~w', [LTxt, RTxt])
		))
	
		%format('Threatened link:~n~p~nAdded ordering:~n~p~n~n', [Link, Ordering]),
	,	resolveThreats((Steps, [Ordering| Orderings], Links, Counters), BetterPlan)
	) 
	% ELSE (we can't find a threat) just return the original plan.
;	BetterPlan = (Steps, Orderings, Links, Counters)
.

% addOrdering/4: given existing orderings, a step, and a causal link, addOrdering produces a tuple that 
% represents an ordering of given step either before the first step of the causal link, or after the second one.

% (alternative 1) Order step BEFORE first step (Step1) of causal link, to prevent possibility of it being after Step1
makeOrdering(Orderings, Step, (Step1, _Step2, _Condition), (Step, Step1)) :-
	possible_after(Orderings, (Step1, Step))
.

% (alternative 2.1) Order step AFTER second step (Step2) of causal link, to prevent possibility of it being before Step2
makeOrdering(Orderings, Step, (_Step1, Step2, _Condition), (Step2, Step)) :-
	Step = (_, Op)
,	\+ framing_schema(Op)	% See paper; we can never order a framing schema after any step.
,	possible_after(Orderings, (Step, Step2))
.

% (alternative 2.2) Order step AFTER second step (Step2) of causal link, to prevent possibility of it being before Step2
makeOrdering(Orderings, Step, (_Step1, Step2, _Condition), (Step2, Step)) :-
	Step = (_, Op)
,	Step2 = (_, Op2)	
,	framing_schema(Op)		% Execption to alternative 2.1: we cannot order a framing schema after a step,
,	framing_schema(Op2)		% unless, UNLESS, the step that we place it after is ALSO a framing schema.
,	possible_after(Orderings, (Step, Step2))
.

% Determines whether given step threatens given causal link.
% This is the case when orderings are insufficient, i.e.
%    - it is possible to execute the given step after first step of causal link
%	 - it is possible to execute the given step before second step of causal link
%	 - the effects of the given step contradict the conditions of the causal link
% Assumes that the condition in the causal link is fully instantiated (using ground/1). If the condition were not ground, this means
% that usually the first step of the link has not been instantiated enough yet by the planning process. If the first step is the start
% step however, it might be a good idea to interpret it as universally quantified (forall).
% Another implementation might use binding constraints (i.e. the step doesn't threaten as long as Location1 != Location2).
threatens(Orderings, ThreateningStep, (Step1, Step2, Condition)) :-
	ThreateningStep = (_Stepname, Operator)
,   ground(Condition)	% ground(+Term) succeeds when Term contains no free variables (SWI-Prolog)
,	operator_effects(Operator, E)
,	member(Em,E)
,   schemas:condition_member(E1, Em)
,	inverse_condition(Condition, E1)	% Are the condition and the effect each others inverse?
,	possible_after(Orderings, (ThreateningStep, Step1))		% Can the threatening step possibly be placed after step 1?
,	possible_after(Orderings, (Step2, ThreateningStep))		% Can step 2 possibly be placed after the threatening step?
,	ignore((
		debugging(threats)
	,	show_step(ThreateningStep, Txt)
	,	show_step(Step1, Txt1)
	,	show_step(Step2, Txt2)
	,	show_condition(Condition, Txt3)
	,	show_condition(E1, Txt4)
	,	prolog_debug:debug(threats, 'THREAT: ~w threatens ~w~n        if it was placed between ~w and ~w~n        Threatening condition is ~w',[Txt, Txt3, Txt1, Txt2,Txt4] )
	))
.

% Check if two conditions are each others inverse (contradict each other).
% By using ==, we "resolve later" (see Russell & Norvig). Only when there is an absolute threat
% (e.g. "at(leChuck, hold)" threatens "not(at(leChuck, hold))"
% do we consider it to be a threat, we delay the resolving of any possible threats
% (e.g. "at(leChuck, hold)" does not yet threaten "not(at(X, hold))".
inverse_condition(condition(true, A1), condition(false, A2)) :-
	A1 == A2.
	% Implement using unifiable/3 and use separation?
	
inverse_condition(condition(false, A1), condition(true, A2)) :-
	inverse_condition(condition(true, A1), condition(false, A2)).
	
% If step2 is not the start step, and 
% if step1 and step2 are different steps, and there is no ordering for the two steps yet,
% and each step ordered after step1 is not before step 2,
% then it is possible to order step1 after step2
% (was: notBefore/2)
possible_after(Orderings, (Step1, Step2)) :-
	\+ startStep(Step1) %    ---> Ivo: added this but I think it is wrong. Haven't thought it through. Commented out for now.
,	Step1 \== Step2
,	\+ ordered(Step1, Step2, Orderings)
,	forall(member((Step1, SomeStep), Orderings), possible_after(Orderings, (SomeStep, Step2)))
.

% ordered/3 reasons about the orderings; two steps S1 and S2 are ordered if:
%	- S1 is not S2
% 	- there is an explicit definition in the orderings that states it. 
%	- there is a step ordered after S1, and before S2. 	
ordered(Step1, Step1, _) :-
	!
,	fail.

ordered(Step1, Step2, Orderings) :-
	member((Step1, Step2), Orderings)
,	!
.

ordered(Step1, Step2, Orderings) :-
	member((Step1, StepOther), Orderings)
, 	ordered(StepOther, Step2, Orderings)
.
	
% Add an element to a set, if and only if it is not already in there.	
add_to_set(X,Set,Set) :-
	memberchk(X,Set)
,	!
.

add_to_set(X,Set,[X|Set]).	


% ---------------------------
% For debugging
% Author: Ivo Swartjes
% ---------------------------

show_plan([]).

show_plan((_Steps, Orderings, _Links, _)) :-
    %show_steps(Steps),
    (	
    	prolog_debug:debug(intermediate_plans, '~n### Plan is now:~n',[])
%    	show_links(Links, TxtL), 
    ,	show_orderings(Orderings, TxtO)
    %Txt = ['LINKS:\n', TxtL, 'ORDERINGS:\n', TxtO],
	,  	Txt = TxtO
   	,	flatten(Txt, Txt2)
	,  	concat_atom(Txt2, ' ', Txt3)
	,  	prolog_debug:debug(intermediate_plans, Txt3,[]) 
   	) 
,	!
;  	prolog_debug:debug(intermediate_plans,'~nERROR in show_plan~n',[]).
    
show_steps([], []).
show_steps([Step | Steps],[TxtListFirst, '\n', TxtListRest]) :-
    show_step(Step, TxtListFirst),
    show_steps(Steps, TxtListRest), !.

show_links([], []).
show_links([Link | Links], ['L:', TxtList1, 'NECESSARY FOR', TxtList2, 'CUZOF', TxtCL, '\n', TxtListRest]) :-
	Link = (S1,S2,C),
	show_step(S1,TxtList1), 
	show_step(S2, TxtList2),
	show_condition(C,TxtCL),
	show_links(Links, TxtListRest).    

show_orderings([], []).	
show_orderings([Ordering | Orderings], ['O:', TxtList1, 'BEFORE', TxtList2, '\n', TxtListRest]) :-
	Ordering = (S1,S2),
	show_step(S1,TxtList1), 
	show_step(S2, TxtList2),
	show_orderings(Orderings, TxtListRest).    
    
%show_steps([]).

%show_steps(Step | Steps) :-
%    show_step(Step), 
    %show_steps(Steps).

show_step(Step, 'start step') :-
	startStep(Step), !.
	
show_step(Step, 'finish step') :-
	finishStep(Step), !.	

show_step((_N,O), Txt) :-
	narrate(O,Txt).

show_condition(condition(Truth, Stuff), Txt) :-
	show_condition_contents(Stuff, StuffTxt),
	Txt = [Truth, StuffTxt], !.

show_condition(Cond, Txt) :-
    %Txt2 = ['pos', PosCond, 'neg', NegCond],
    Txt = [Cond].
    
show_condition_contents([], []) :- !.
show_condition_contents([L1|L], Txt) :-
	show_fact(L1, Txt1), 
	show_condition_contents(L, Txt2),
	Txt = [Txt1, 'and', Txt2],
	!.

show_condition_contents(Cond, Txt) :-
	show_fact(Cond, Txt).
	
show_fact(Cond, Txt) :-
	Cond =.. [F, S, P, O],
	narrate((S,P,O), Txt1),
	Txt = [F, '(', Txt1, ')'].

show_fact(Cond, Txt) :-
	Cond =.. [F, S, P, O, G],
	narrate((S,P,O,G), Txt1),
	Txt = [F, '(', Txt1, ')'].
   