% File which loads in the action database and contains the main functionality of the action
% database.


:- module(action_db,
	[ query/1, 						% ??? Something with math, unp and stuff.
	  query/3,						% ?Subject, ?Predicate, ?Object
	  validateAction/2,				% +Action, -FailedPreconditionList
	  applyActionInterEffects/1,	% +Action
	  applyActionEffects/1,			% +Action

	  dissectAction/8,				% ?Action, ?ActionAguments x7 (see directly below)
	  getSuperAction/2,				% ?Action, ?SuperAction
	  primitiveAction/1, 			% ?Action
	  getInterDuration/2, 			% ?Action, ?InterDuration
	  getDuration/2, 				% ?Action, ?Duration
	  getPreconditions/2,			% ?Action, ?Preconditions
	  getInterEffects/2, 			% ?Action, ?getInterEffects
	  getEffects/2,					% ?Action, ?getEffects
	  
	  getAllEffects/2,				% ?Action, ?getAllEffects			(added by Kruizinga)
	  % getAllPreconditions/2,		% ?Action, ?getAllPreconditions		(added by Kruizinga)

	  validatePreconditions/2,		% +Preconditions, -InvalidPreconditions
	  validPreconditionBlock/1		% +PreconditionBlock
		  
	]).

% Operators of the action database
:- op(801, fx, action).
:- op(790, xfy, superAction).
:- op(790, xfy, primitiveAction).
:- op(790, xfy, interDuration).
:- op(790, xfy, duration).
:- op(790, xfy, preconditions).
:- op(790, xfy, interEffects).
:- op(790, xfy, effects).
:- op(544, fx, add).
:- op(543, xfy, del).
:- op(301, xfy, and).
:- op(401, xfy, nextBlock).
:- op(201, fx, unp).

% Load in knowledge base
:- include('knowledgebase.pl').

% Load in action database
:- include('actiondb/action.pl').

% Modules
:- use_module(semweb(rdf_db)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Query EXPANSION     %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This predicate allows for writing down rdf queries in a friendly
% name-space fashion.
%
% Can not be made to work properly: Is only used when these functions
% are called from the prolog-command line. But this will not be the
% case.

%:- multifile
%	use:goal_expansion/2.

%:- rdf_meta
%	query(r,r,o).


%%%%%%%%%%%%%%%%%%%%%
% Main functionality:
%
% - Action Validation
% - OWL database access
%%%%%%%%%%%%%%%%%%%%%


%%%%%%
% Query(+SinglePreconditionClause)
%
% Queries if a SinglePreconditionClause is true

% Query the rule-set.
query((S, P, O)):-
	query(S,P,O). % remove brackets.

% Query for mathematics
query(math(X)):-
	call(X).

% Query the rule-set.
%
% For (probably irrelevant) speed issues, the rulebase is queried
% directly. To enable general rules use:
%
% 	query(S, R, O):- rule(S,R,O).

query(S, swcr:RuleName, O):-
	% Perform the rule from the rulebase
	rule(S, swcr:RuleName, O).

query(S, owlr:RuleName, O):-
	% perform rule from rulebase
	rule(S, owlr:RuleName, O).

% Query owl database
% Changed hasChangeLiterals to hasChangeLiterals1 in which no cut ! is used to enable searching.
query(S,P,O):-
	hasChangeLiterals1(S,P,O).

%%%%%%
% hasChangeLiterals does a 'has' query on the owl database, but replaces 
% any literals with their respective values.
%
% Explanation: The semantic web package stores booleans as:
% 	literal(type('http://www.w3.org/2001/XMLSchema#boolean', true)
% and integers as
% 	literal(type('http://www.w3.org/2001/XMLSchema#integer', '80')
% 
% But we want to get just: true or 80.
hasChangeLiterals(S,P,O):-
	(
		possibleLit(S),
		possibleLit(O), !,
		has(S1, P, O1),
		extractLitVal(S1,S),
		extractLitVal(O1,O)
	;
		not(possibleLit(S)),
		possibleLit(O), !,
		has(S, P, O1),
		extractLitVal(O1,O)
	;
		possibleLit(S),
		not(possibleLit(O)), !,
		has(S1, P, O),
		extractLitVal(S1, S)
	;
		not(possibleLit(S)),
		not(possibleLit(O)), !,
		has(S,P,O)
	).

hasChangeLiterals1(S,P,O):-
	(
		possibleLit(S),
		possibleLit(O),
		has(S1, P, O1),
		extractLitVal(S1,S),
		extractLitVal(O1,O)
	;
		not(possibleLit(S)),
		possibleLit(O),
		has(S, P, O1),
		extractLitVal(O1,O)
	;
		possibleLit(S),
		not(possibleLit(O)),
		has(S1, P, O),
		extractLitVal(S1, S)
	;
		not(possibleLit(S)),
		not(possibleLit(O)),
		has(S,P,O)
	).

%%%%%
% has(+Subject, +Predicate, +Object)
%
% queries if a triple is valid according to the owl database.
has(S, P, O):-
	rdf_db:rdf_global_id(S, S1), % deal with namespaces
	rdf_db:rdf_global_id(P, P1),
	rdf_db:rdf_global_id(O, O1),
	owl:owl_inference(S1, P1, O1). % pose the query	
	
%%%%%
% possibleLit(+Term) 
%
% succeeds if Term can be a literal
possibleLit(L):-
	(
		var(L), !   	% a variable can be a literal
	;
		integer(L), !	% an integer is a literal (according to owl.pl)
	;
		L == true, !	% a boolean is a literal
	;
		L == false, !	% a boolean is a literal
	;
		atom(L),
		L \= _B:_C, !  	% a string which does not have a division between 
	;
		L = literal(type(_Type, _Value)), ! % an instantiated literal
	).

%%%%%%
% extractLitVal(+SemwebPackageLit, -Value)
% literal extractor
extractLitVal(literal(type('http://www.w3.org/2001/XMLSchema#int', Val)), IntVal):-
	atom_to_term(Val, IntVal, _),
	integer(IntVal), !.

extractLitVal(literal(type('http://www.w3.org/2001/XMLSchema#boolean', Val)), Val):- !.

extractLitVal(literal(type('http://www.w3.org/2001/XMLSchema#string', Val)), Val):- !.

extractLitVal(X,X).	

%%%%%%
% applyActionInterEffects(+Action)
%
% applies the interEffects of an action.
%
% WARNING: No check whatsoever is made if the action is valid.
% Do this beforehand.
applyActionInterEffects(A):-
	nonvar(A),
	getInterEffects(A, IE), !,
	applyEffects(IE), !,
	(
		getSuperAction(A, none), ! % There is no superaction. We are done.
	;
		getSuperAction(A, SA), !, 
		applyActionInterEffects(SA)
	).

%%%%%%
% applyActionEffects(+Action)
%
% applies the effects of an action.
%
% WARNING: No check whatsoever is made if the action is valid or if
% the interEffects already have been applied.
applyActionEffects(A):-
	nonvar(A),
	getEffects(A, E), !,
	applyEffects(E), !,
	(
		getSuperAction(A, none), ! % There is no superaction. We are done.
	;
		getSuperAction(A, SA), !, 
		applyActionEffects(SA)
	).

%%%%%%
% getAllEffects(+Action, -Effects)
%
% Effects are all effects of the action and its superactions
% (kruizinga 7 nov 2006)
getAllEffects(A, [E, SuperE]) :-
	getEffects(A, E),
	(
		getSuperAction(A, none) ;
		getSuperAction(A, SA),
		getAllEffects(SA, SuperE)
	).

%%%%%
% validateAction(+PrimitiveAction, -FalsePreconditionList)
%
% This functions needs an action as an input, and returns a
% list with preconditions that do not hold.
%
% Notice that validateAction(+Action, []) is true iff the action
% is valid.
validateAction(A, TUP):-
	nonvar(A), % The action must be instantiated
	primitiveAction(A), !, % is this action primitive?
	getPreconditions(A, P),
	validatePreconditions(P, UP), !, % UP = list of Untrue Preconditions
	getSuperAction(A, SA),
	validateSuperAction(SA, SAUP), !,
	append(UP, SAUP, TUP), !. % store the Total Untrue Preconditions.
	
%%%%%
% validateSuperAction(+Action, -FalsePreconditionList)
%
% This function checks if the preconditions for any given action are valid.

% A non-recurdive clause for the upper action
validateSuperAction(A, UP):-
	upperAction(A), % There is no superaction for A.
	getPreconditions(A,P),
	validatePreconditions(P,UP), !.

% A recursive function for the non-upper action
validateSuperAction(A, TUP):-
	getPreconditions(A,P),
	validatePreconditions(P,UP), !,
	getSuperAction(A,SA),
	validateSuperAction(SA, SAUP), !,
	append(UP, SAUP, TUP), !.


%%%%%
% validatePreconditions(+Preconditions, -UntruePreconditionList)
%
% This function determines which precondition blocks are true

% There is only one precondition block left.
validatePreconditions(P, UP):-
	not(containsOperator(P, nextBlock)), !,% a sole precondition block is left
	(
		validPreconditionBlock(P), !,
		UP = []
	;
		UP = [P]
	).

% There are more precondition blocks
validatePreconditions(P nextBlock RestP, UP):-
	not(containsOperator(P, nextBlock)), !, % P is ONE preconditionBlock
	validatePreconditions(RestP, RestUP),	% first check the rest of the preconditions
	(
		validPreconditionBlock(P), !,
		append([], RestUP, UP), !   	% Valid. Append empty list
	;
		append([P], RestUP, UP), !	% not valid. Append P.
	).


%%%%%
% validatePreconditionBlock(+PreconditionBlock)
%
% Succeeds if the precondition block is indeed valid.

% There is no precondition block
validPreconditionBlock(none):- !.

% If a precondition block consists of multiple elements, split them.
validPreconditionBlock(P1 and P2):-
	validPreconditionBlock(P1),
	validPreconditionBlock(P2).

% If a precondition block is one precondition, query the precondition.
validPreconditionBlock(P):-
	query(P).






%%%%%
% applyEffects(E)
%
% applies the effects to the database.
applyEffects(none):- !.
applyEffects(add A del D):-
	addEffects(A), !,
	delEffects(D), !.

%%%%%
% addEffects(+Effects)
%
% adds effects to the database.

% There is no effect to add.
addEffects(none):- !.

% for a single effect:
addEffects((S, P, O)):-
	nonvar(S),
	nonvar(P),
	nonvar(O),
	rdf_db:rdf_global_id(S, S1), % deal with namespaces
	rdf_db:rdf_global_id(P, P1),
	rdf_db:rdf_global_id(O, O1),
	rdf_edit:rdfe_transaction(rdfe_assert(S1, P1, O1)), !.

addEffects(E1 and E2):-
	addEffects(E1), !,
	addEffects(E2), !.

%%%%%
% delEffects(+Effects)
%
% deletes effects from the database.

% There is no effect to delete.
delEffects(none):- !.

% for a single effect:
delEffects((S, P, O)):-
	nonvar(S),
	nonvar(P),
	nonvar(O),
	rdf_db:rdf_global_id(S, S1), % deal with namespaces
	rdf_db:rdf_global_id(P, P1),
	rdf_db:rdf_global_id(O, O1),
	rdf_edit:rdfe_transaction(rdfe_retractall(S1, P1, O1)), !.

delEffects(E1 and E2):-
	delEffects(E1), !,
	delEffects(E2), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions for extracting information from the action database
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%
% getSuperAction(?Action, ?SuperAction)
%
% This function gets the superaction of the given action.
getSuperAction(A, SA):-
	dissectAction(A, SA, _PA, _ID, _D, _P, _IE, _E).

%%%
% upperAction(+Action)
%
% queries if the action is the top-action
upperAction(A):-
	dissectAction(A, none, _PA, _ID, _D, _P, _IE, _E).

%%%
% primitiveAction(?Action) succeeds if Action is primitive or returns a 
% primitive action.
primitiveAction(A):-
	dissectAction(A, _, true, _, _, _, _, _).	

getInterDuration(A, ID):-
	dissectAction(A, _SA, _PA, ID, _D, _P, _IE, _E).

getDuration(A, D):-
	dissectAction(A, _SA, _PA, _ID, D, _P, _IE, _E).

%%%
% getPreconditions(?Action, ?Preconditions)
%
% This function gets the preconditions of an action. Preconditions have the form:
%
% 	p1 and p2 and p3 nextBlock p4 and p5
%
% If an action does not have preconditions, Preconditions become
%
% 	none
getPreconditions(A, P):-
	dissectAction(A, _SA, _PA, _ID, _D, P, _IE, _E).


getInterEffects(A, IE):-
	dissectAction(A, _SA, _PA, _ID, _D, _P, IE, _E).

getEffects(A, E):-
	dissectAction(A, _SA, _PA, _ID, _D, _P, _IE, E).

%%%
% dissectAction(+Action, 
% 		-SuperAction
% 		-PrimitiveAction
% 		-InterDuration
% 		-Duration
% 		-Preconditions
% 		-InterEffects
% 		-Effects).
%
% This action dissects an action into its various constituents.
% If an argument is not defined, it returns 'none'.
%

% For a completely described action
dissectAction(A, SA, PA, ID, D, P, IE, E):-
	action
		A
	superAction
		SA
	primitiveAction
		PA
	interDuration
		ID
	duration
		D
	preconditions
		P
	interEffects
		IE
	effects
		E.

% No effects nor intereffects specified
dissectAction(A, SA, PA, ID, D, P, none, none):-
	action
		A
	superAction
		SA
	primitiveAction
		PA
	interDuration
		ID
	duration
		D
	preconditions
		P,
	not(containsOperator(P, interEffects)),
	not(containsOperator(P, effects)).
	
% Here not inter* are specified.
dissectAction(A, SA, PA, none, D, P, none, E):-
	action
		A
	superAction
		SA
	primitiveAction
		PA
	duration
		D
	preconditions
		P
	effects
		E, 
	not(containsOperator(PA, interDuration)),
	not(containsOperator(P, effects)).

% No duration or interDuration is specified
dissectAction(A, SA, PA, none, none, P, IE, E):-
	action
		A
	superAction
		SA
	primitiveAction
		PA
	preconditions
		P
	interEffects
		IE
	effects
		E, 
	not(containsOperator(PA, interDuration)),
	not(containsOperator(PA, effects)).

% No duration interDuration nor interEffects are specified
dissectAction(A, SA, PA, none, none, P, none, E):-
	action
		A
	superAction
		SA
	primitiveAction
		PA
	preconditions
		P
	effects
		E,
	not(containsOperator(PA, interDuration)),
	not(containsOperator(PA, duration)),
	not(containsOperator(P, interEffects)).

% No duration, interDuration, interEffects not effects are specified.
dissectAction(A, SA, PA, none, none, P, none, none):-
	action
		A
	superAction
		SA
	primitiveAction
		PA
	preconditions
		P, 
	not(containsOperator(PA, interDuration)),
	not(containsOperator(PA, duration)),
	not(containsOperator(P, interEffects)),
	not(containsOperator(P, effects)).

% Only an action, superAction and primitiveAction
dissectAction(A, SA, PA, none, none, none, none, none):-
	action
		A
	superAction
		SA
	primitiveAction
		PA,
	not(containsOperator(PA, preconditions)),	
	not(containsOperator(PA, interDuration)),
	not(containsOperator(PA, duration)),
	not(containsOperator(PA, interEffects)),
	not(containsOperator(PA, effects)).

%%%%%%%%%%%%%%
% Miscellaneous functions
%%%%%%%%%%%%%%

% Negated precondition
query(unp P):- 
	% call validPreconditionBlock(P). Do not use query(P) because
	% P might be a composite precondition.
	not(validPreconditionBlock(P)).

% containsOperator(+Sentence, +Operator)
%
% checks if operators are contained in the Sentence
% Quite involved because of operator priorities.
%
% USE THIS FUNCTION WITH CAUTION. DOES NOT ALWAYS WORK
% BECAUSE OF OPERATOR PRIORITIES

containsOperator(_A nextBlock _B, nextBlock).
containsOperator(_A and _B, and).


% For the action definition
containsOperator(action _A, action).
containsOperator(_A superAction _B, superAction).
containsOperator(_A primitiveAction _B, primitiveAction).
containsOperator(_A interDuration _B, interDuration).
containsOperator(_A duration _B, duration).
containsOperator(_A preconditions _B, preconditions).
containsOperator(_A interEffects _B, interEffects).
containsOperator(_A effects _B, effects).

containsOperator(action A, Operator):-
	containsOperator(A, Operator).

containsOperator(_A superAction B, Operator):-
	containsOperator(B, Operator).

containsOperator(_A primitiveAction B, Operator):-
	containsOperator(B, Operator).

containsOperator(_A interDuration B, Operator):-
	containsOperator(B, Operator).

containsOperator(_A duration B, Operator):-
	containsOperator(B, Operator).

containsOperator(_A preconditions B, Operator):-
	containsOperator(B, Operator).

containsOperator(_A interEffects B, Operator):-
	containsOperator(B, Operator).
