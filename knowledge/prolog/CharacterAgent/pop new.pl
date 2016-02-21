% Partial Order planner
% @author Kruizinga
% 22-03-2007 - 15-06-2007 - 1-10-2007 - 2-11-2007

% this file is loaded by planner.pl

% maximum number to use for counters
maxInt(10000).

counterDepth((Depth, _Cost, _Improvs), Depth).
counterCost((_Depth, Cost, _Improvs), Cost).
counterImprovs((_Depth, _Cost, Improvs), Improvs).
counterZero((0,0,0)).

% these functions use an older version of the pop function that I removed
% in this file there is no good function yet to call from the java control software
/*
idPopI(MaxDepth, MaxImprovs, PosGoal, NegGoal, Plan) :-
	popIterator(MaxDepth, MaxImprovs, 0, PosGoal, NegGoal, Plan).

idPopIC(MaxDepth, MaxImprovs, PosGoal, NegGoal, Plan) :-
	popIteratorC(MaxDepth, MaxImprovs, 0, PosGoal, NegGoal, Plan).

popIteratorI(MaxDepth, MaxImprovs, IDepth, PosGoal, NegGoal, Plan) :-
	idPopC(MaxDepth, IDepth, PosGoal, NegGoal, Plan);
	(IDepth1 is IDepth + 1,
	IDepth1 =< MaxImprovs,
	popIteratorI(MaxDepth, MaxImprovs, IDepth1, PosGoal, NegGoal, Plan)).

% idPop/4 MaxDepth, PosGoal, NegGoal, ?Plan
idPop(MaxDepth, MaxImprovs, PosGoal, NegGoal, PP, Plan) :-
	popIterator(MaxDepth, MaxImprovs, 0, PosGoal, NegGoal, PP, Plan).

idPopC(MaxDepth, MaxImprovs, PosGoal, NegGoal, Plan) :-
	popIteratorC(MaxDepth, MaxImprovs, 0, PosGoal, NegGoal, Plan).

popIterator(MaxDepth, MaxImprovs, Depth, PosGoal, NegGoal, Plan) :-
	maxInt(MaxInt),
	pop((Depth, MaxInt, MaxImprovs), PosGoal, NegGoal, Plan);
	(Depth1 is Depth + 1,
	Depth1 =< MaxDepth,
	popIterator(MaxDepth, MaxImprovs, Depth1, PosGoal, NegGoal, Plan)).

popIteratorC(MaxDepth, MaxImprovs, Depth, PosGoal, NegGoal, Plan) :-
	maxInt(MaxInt),
	pop((MaxInt, Depth, MaxImprovs), PosGoal, NegGoal, Plan);
	(Depth1 is Depth + 1,
	Depth1 =< MaxDepth,
	popIterator(MaxDepth, Depth1, PosGoal, NegGoal, Plan)).
*/

% this function always makes a new minimalPlan and uses the Type of iterator given
pop(Type, Max, PosGoal, NegGoal, Plan) :-
	makeMinimalPlan(PosGoal, NegGoal, MinimalPlan),
	iterator(Type, Max, MinimalPlan, Plan).	
	%improvIterator(Type, Max, MinimalPlan, Plan).	

% iterator over depth
iterator(depth, (D, C, I), OldPlan, Plan) :-
	findplan((D, C, I), OldPlan, Plan);
	(D1 is D + 1,
	iterator(depth, (D1, C, I), OldPlan, Plan)).

% iterator over cost
iterator(cost, (D, C, I), OldPlan, Plan) :-
	findplan((D, C, I), OldPlan, Plan);
	(C1 is C + 1,
	iterator(cost, (D, C1, I), OldPlan, Plan)).

% iterate over improvisations as well as depth or cost, set Type to either of these
improvIterator(Type, (D, C, I), OldPlan, Plan) :-
	iterator(Type, (D, C, I), OldPlan, Plan);
	(I1 is I + 1,
	improvIterator(Type, (D, C, I1), OldPlan, Plan)).

% startStep defines the startStep.
startStep((s, dummy)).
%	[
%	class(action),
%	head([
%		type(start)
%		]),
%	duration(0),
%	posPreconditions([]),
%	negPreconditions([]),
%	posEffects(dummy),
%	posEffects(dummy)
%	])).

/*
schema([
						class(action),
						head([
							type(start)
							]),
						duration(0),
						posPreconditions(_),
						negPreconditions(_),
						posEffects(_),
						negEffects(_)
	]).
*/

% makeMinimalPlan/3 +PosGoal, +NegGoal, ?Plan
% makeMinimalPlan returns an initial plan containing a start and a finish step
makeMinimalPlan(PosGoal, NegGoal,
	([FinishStep], [(StartStepname, f)], [], CounterZero)) :-
	counterZero(CounterZero),
	startStep((StartStepname, _StartStepHead)),
	FinishStep = (f, [
						class(action),
						head([
							type(finished)
							]),
						duration(0),
						posPreconditions(PosGoal),
						negPreconditions(NegGoal),
						posEffects([]),
						negEffects([])
				]).

findPlan(Max, Plan, NewPlan) :-
	choosePrecondition(Plan, Stepname, PosCondition, NegCondition) ->
	(
		chooseOperator(Max, Plan, Stepname, PosCondition, NegCondition, Plan2),
		resolveThreats(Plan2, Plan3),
		findPlan(Max, Plan3, NewPlan)
	) ;
	NewPlan = Plan.

choosePrecondition((Steps, _Orderings, Links, _Depth), Stepname, PP, none) :-
	member((Stepname, StepOperator), Steps),
	getFromSchema(StepOperator, posPreconditions(PPs)),
	member(PP, PPs),
	\+ memberchk((_X, Stepname, PP, none), Links).

choosePrecondition((Steps, _Orderings, Links, _Depth), Stepname, none, NP) :-
	member((Stepname, StepOperator), Steps),
	getFromSchema(StepOperator, negPreconditions(NPs)),
	member(NP, NPs),
	\+ memberchk((_X, Stepname, none, NP), Links).

% chooseOperator/4 +CurrentPlan, + Step, +Condition, -BetterPlan
% choose either a step from the plan or a new operator
chooseOperator(Max, CurrentPlan, AskStepname, PosCondition, NegCondition, NewPlan) :-
	chooseStart(Max, CurrentPlan, AskStepname, PosCondition, NegCondition, NewPlan);
	chooseStep(CurrentPlan, AskStepname, PosCondition, NegCondition, NewPlan);
	chooseAction(Max, CurrentPlan, AskStepname, PosCondition, NegCondition, NewPlan);
	chooseImprovisation(Max, CurrentPlan, AskStepname, PosCondition, NegCondition, NewPlan).

conditionCost((_S, R, O), D) :-
	(R = 'http://www.owl-ontologies.com/StoryWorldCore.owl#length',
	O = literal(type('http://www.w3.org/2001/XMLSchema#int', L))
	%,format('length is ~p~n', L)
	)
	->
	atom_to_term(L, D, _Bindings)
	; D = 0.

% chooseStart
% reuse the start (current) state
chooseStart(Max, (Steps, Orderings, Links, (Depth, Cost, Improvs)), AskStepname, PosCondition, none, NewPlan) :-
	query(PosCondition),
	conditionCost(PosCondition, D),
	counterCost(Max, MaxCost),
	Cost1 is Cost + D,
	Cost1 =< MaxCost,
	startStep((NewStepname, _NewStepOperator)),
	NewPlan = (Steps, [(NewStepname, AskStepname) | Orderings], [(NewStepname, AskStepname, PosCondition, none) | Links], (Depth, Cost1, Improvs))
	%,format('Start state new link:~n~p~n~n', (NewStepname, AskStepname, PosCondition, none))
	.
	
% chooseStart
% reuse the start (current) state
chooseStart(_Max, (Steps, Orderings, Links, Counters), AskStepname, none, NegCondition, NewPlan) :-
	unpQuery(NegCondition),
	startStep((NewStepname, _NewStepOperator)),
	NewPlan = (Steps, [(NewStepname, AskStepname) | Orderings], [(NewStepname, AskStepname, none, NegCondition) | Links], Counters)
	%,format('Start state new link:~n~p~n~n', (NewStepname, AskStepname, none, NegCondition))
	.

% chooseStep
% reuse a step in the plan that achieves the precondition
chooseStep((Steps, Orderings, Links, Counters), AskStepname, PosCondition, NegCondition, NewPlan) :-
	member((Stepname, StepOperator), Steps),
	notBefore(Orderings, (AskStepname, Stepname)),
	effectMember(PosCondition, NegCondition, (Stepname, StepOperator)),
	NewPlan = (Steps, [(Stepname, AskStepname) | Orderings], [(Stepname, AskStepname, PosCondition, NegCondition) | Links], Counters)
	%,format('Old step new link:~n~p~n~n', (Stepname, AskStepname, PosCondition, NegCondition))
	.

% chooseAction
% put an action operator in the plan that achieves the precondition
chooseAction(Max, (Steps, Orderings, Links, (Depth, Cost, Improvs)), AskStepname, PosCondition, NegCondition, NewPlan) :-
	counterDepth(Max, MaxDepth),
	counterCost(Max, MaxCost),
	Depth1 is Depth + 1,
	Depth1 =< MaxDepth,
	schema(NewOperator),
	getFromSchema(NewOperator, class(action)),
	NewStep = (Depth, NewOperator), %Depth is used as the name for a new operator
	effectMember(PosCondition, NegCondition, NewStep),
	%checkAgent(NewStep),
	getFromSchema(NewOperator, duration(D)),
	Cost1 is Cost + D,
	Cost1 =< MaxCost,
	NewPlan = ([NewStep| Steps], [(Depth, AskStepname) | Orderings], [(Depth, AskStepname, PosCondition, NegCondition) | Links], (Depth1, Cost1, Improvs))
	%,format('New step new link:~n~p~n~n', (Depth, AskStepname, PosCondition, NegCondition))
	.

% chooseImprovisation
% put an improvisation operator in the plan that achieves the precondition
chooseImprovisation(Max, (Steps, Orderings, Links, (Depth, Cost, Improvs)), AskStepname, PosCondition, NegCondition, NewPlan) :-
	counterDepth(Max, MaxDepth),
	counterImprovs(Max, MaxImprovs),
	Depth1 is Depth + 1,
	Depth1 =< MaxDepth,
	Improvs1 is Improvs + 1,
	Improvs1 =< MaxImprovs,
	schema(NewOperator),
	getFromSchema(NewOperator, class(improvisation)),
	NewStep = (Depth, NewOperator), %Depth is used as the name for a new operator
	effectMember(PosCondition, NegCondition, NewStep),
	getFromSchema(NewOperator, head(H)),
	getFromHead(H, agens(Agens)),
	query((myself, iam, Agens)),
	checkAgent(NewStep),
	NewPlan = ([NewStep| Steps], [(Depth, AskStepname) | Orderings], [(Depth, AskStepname, PosCondition, NegCondition) | Links], (Depth1, Cost, Improvs1))
	%,format('New improvisation new link:~n~p~n~n', (Depth, AskStepname, PosCondition, NegCondition))
	.

% checkAgent checks whether Agent is the agent itself. Is also makes sure the Agent does not bind any other variables to its own "name"
checkAgent((_Stepname, Operator)) :-
	query((myself, iam, Agens)),
	getFromSchema(Operator, head(H)),
	getFromHead(H, agens(Agens))
	%,\+(groundMember(Agens, Vars)) % Maybe this is not checked again later. It would have to be rechecked when a step is reused.
	.
	
effectMember(PosCondition, none, (_Stepname, Operator)) :-
	getFromSchema(Operator, posEffects(PEs)),
	member(PosCondition, PEs).

effectMember(none, NegCondition, (_Stepname, Operator)) :-
	getFromSchema(Operator, negEffects(NEs)),
	member(NegCondition, NEs).

resolveThreats((Steps, Orderings, Links, Counters), BetterPlan) :-
	(
	member((Stepname, StepHead), Steps),
	member(Link, Links),
	threatens(Orderings, (Stepname, StepHead), Link)
	) -> (
	addOrdering(Orderings, Stepname, Link, Ordering),
	%format('Threatened link:~n~p~nAdded ordering:~n~p~n~n', [Link, Ordering]),
	resolveThreats((Steps, [Ordering| Orderings], Links, Counters), BetterPlan)
	) ; 
	BetterPlan = (Steps, Orderings, Links, Counters).

addOrdering(Orderings, Step, (Step1, _Step2, _PosCondition, _NegCondition), (Step, Step1)) :-
	notBefore(Orderings, (Step1, Step)).

addOrdering(Orderings, Step, (_Step1, Step2, _PosCondition, _NegCondition), (Step2, Step)) :-
	notBefore(Orderings, (Step, Step2)).

threatens(Orderings, (Stepname, Operator), (Step1, Step2, PosCondition, NegCondition)) :-
	getFromSchema(Operator, posEffects(PEs)),
	getFromSchema(Operator, negEffects(NEs)),
	notBefore(Orderings, (Stepname, Step1)),
	notBefore(Orderings, (Step2, Stepname)),
	(
		(ground(PosCondition), groundMember(PosCondition, NEs));
		(ground(NegCondition), groundMember(NegCondition, PEs))
	).

groundMember(TestElement, [Element| List]) :-
	(ground(Element), TestElement = Element);
	groundMember(TestElement, List).

notBefore(Orderings, (Step1, Step2)) :-
	Step1 \= Step2,
	\+ memberchk((Step1, Step2), Orderings),
	forall(member((Step1, SomeStep), Orderings), notBefore(Orderings, (SomeStep, Step2))).

%beforeTrans(_Orderings, (Step, Step)).

%beforeTrans(Orderings, (Step1, Step2)) :-
%	beforeTransRecursion(Orderings, (Step1, Step2)).

%beforeTransRecursion(Orderings, (Step1, Step2)) :-
%	(member((Step1, Step2), Orderings);
%		(member((Step1, Step3), Orderings),
%		beforeTransRecursion(Orderings, (Step3, Step2))
%	)).
