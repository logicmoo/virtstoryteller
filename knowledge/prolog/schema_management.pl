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

% ----------------------------------------------------------------------
% Schema management module
% Provides access predicates and validation for schema definitions
%
% based partly on earlier version by Jasper Uijlings and Edze Kruizinga
%
% Note: schema definitions are not stored in the database exactly as they
% 		are specified in the prolog files that define them:
%	 -	Namespaces are expanded;
%	 -	Effects (of operator schemas) are extended with conditions that
%		specify the operator descriptions themselves.
% ----------------------------------------------------------------------

:- module(schemas,
	[	
		% Schema ontology
		schema/1					% ?Schema
	,	operator_schema/1
	,	action_schema/1
	,	goal_schema/1
	,	event_schema/1
	,	framing_schema/1
	,	inference_schema/1
	,	belief_schema/1
	,	expectation_schema/1
	,	thread_schema/1
	,	action_selection_schema/1

		% Schema access predicates
	,	schema_kind/2
	
	,   schema_type/2
	,   operator_duration/2
	,   schema_preconditions/2
	,   condition_member/2
	,   condition_member/3	
	,   operator_effects/2
	,	operator_effect/5
	,	operator_duration/2
	,   goal_success_conditions/2
	,   goal_failure_conditions/2
	,	goal_urgency/2
	,	framing_scope_all/1
	,	framing_scope_personal/1
	,	framing_scope_hidden/1
	,   schema_agens/2
	,   schema_patiens/2
	,   schema_target/2
	,   schema_instrument/2
	,   schema_opponent/2

		% Schema validation and execution
   	,	validate_schema/2					% +Schema, -FailedPreconditionList,
	,	validate_condition/1                % +Condition
	,	apply_operator_effects/1			% +Operator
	,	validate_goal_failure_conditions/1	% +Schema
	,	validate_goal_success_conditions/1	% +Schema
	,	in_character/1
	,	out_of_character/1
    
	]).

% Modules
:- use_module(library(semweb/rdf_db)).
:- use_module('knowledgebase.pl').
:- use_module('owl.pl').


%%%%%%%%%%%%%%%%%%%%%
% Main functionality:
%
% - Operator Validation
% - OWL database access
%%%%%%%%%%%%%%%%%%%%%

:- rdf_meta
	action_selection(t, r).
%	thread_schema(t).   % Experimental.


% schema/1 clauses are defined in separate files for each story domain
:- multifile 
	goal_schema/1
,	belief_schema/1
,	expectation_schema/1
,	event_schema/1
,	action_schema/1
,	framing_schema/1
,	inference_schema/1
,	thread_schema/1
,	action_selection_schema/1
,	schema/1.

% Term and goal expansion so user of module does not have to worry about RDF URIs being expanded or prefixed.
% i.e., there is no semantic difference between fabula:agens and http://....#agens
:- multifile
	user:term_expansion/2
,	user:goal_expansion/2
.
	
:- dynamic
	user:term_expansion/2
,	user:goal_expansion/2
.


% ------------------------
% Schema ontology
% ------------------------
schema(S) :- 
    operator_schema(S)
;   goal_schema(S)
;	thread_schema(S)
;	action_selection_schema(S)
.    

% Operator: those schemas that can be "done" (either physically or mentally) because they have effects
operator_schema(S) :- 
    action_schema(S)
;   framing_schema(S)
;	inference_schema(S)
;   event_schema(S)
;   belief_schema(S)
;	expectation_schema(S)
.

% Is given schema in-character (IC)?
% IC schemas are schemas that do model something at the character level (e.g., actions, goals)
in_character(S) :-
	\+ out_of_character(S)
.

% Is given schema out-of-character (OOC)?
% OOC schemas are schemas that do not model anything at the character level (but at the actor level)
out_of_character(S) :-
	framing_schema(S)
;	event_schema(S)
.

% Kind of schema
schema_kind(S, action) 				:-  action_schema(S).
schema_kind(S, event) 				:-  event_schema(S).
schema_kind(S, goal) 				:-  goal_schema(S).
schema_kind(S, framing)				:-	framing_schema(S).
schema_kind(S, inference)			:-	inference_schema(S).
schema_kind(S, belief) 				:-  belief_schema(S).
schema_kind(S, expectation) 		:-  expectation_schema(S).
schema_kind(S, thread) 				:-  thread_schema(S).
schema_kind(S, action_selection) 	:-	action_selection_schema(S).

% ------------------------------------------------------------------
% Term expansion (so that you can mix prefixed and full URIs)
%   Little explanation (swartjes):
%   when SWI-Prolog reads in the .pl files as a collection of
%   Prolog terms, it will try to unify each and every one of them
%   at compile time with user:term_expansion(T1,T2) predicates like
%   the one below. If it succeeds, in stead of asserting T1 in its
%   predicate DB, prolog asserts T2. It does this at compile time.
%
%	the below term_expansion tries to expand schema definitions into
% 	a form where every prefixed URI occurring in the schema, 
%	is replaced by a full URI. E.g. 
%	lolli:linda  -->  http://.../Lollipop.owl#linda
%
%	Apparently, the directive rdf_meta can handle this too:
%
%	:- rdf_meta  action_schema(t).
%
%   (untested)
%
%	Another thing the term expansion predicates do is add implied
%	default information to the schemas. For instance, it adds to the
%	effects of an action schema the fabula describing this action
%	(i.e., the implied effect of an action is that the action is done)
%	
% ------------------------------------------------------------------

% Expansion of action schema
user:term_expansion(action_schema(S), action_schema(T)) :-
	expand_term(schema(S), schema(T)).
	
% Expansion of event schema
user:term_expansion(event_schema(S), event_schema(T)) :-
	expand_term(schema(S), schema(T)).	
	
% Expansion of belief schema
user:term_expansion(belief_schema(S), belief_schema(T)) :-
	expand_term(schema(S), schema(T)).	
	
% Expansion of expectation schema
user:term_expansion(expectation_schema(S), expectation_schema(T)) :-
	expand_term(schema(S), schema(T)).	

% Expansion of goal schema	
user:term_expansion(goal_schema(S), goal_schema(T)) :-
	expand_term(schema(S), schema(T)).	
	
% Expansion of framing schema	
user:term_expansion(framing_schema(S), framing_schema(T)) :-
	expand_term(schema(S), schema(T)).

% Expansion of inference schema	
user:term_expansion(inference_schema(S), inference_schema(T)) :-
	expand_term(schema(S), schema(T)).
	
% Expansion of thread schema	
user:term_expansion(thread_schema(S), thread_schema(T)) :-
	expand_term(schema(S), schema(T)).	
	
% Expansion of conditional thread schemas
user:term_expansion((thread_schema(S) :- Body), (thread_schema(T) :- Body)) :-
	expand_term(schema(S), schema(T)).		
	
% Expansion of action selection schema	
user:term_expansion(action_selection_schema(S), action_selection_schema(T)) :-
	expand_term(schema(S), schema(T)).	
	
% Expansion of conditional action selection schemas
user:term_expansion((action_selection_schema(S) :- Body), (action_selection_schema(T) :- Body)) :-
	expand_term(schema(S), schema(T)).			

% Basic term expansion for schema
user:term_expansion(schema(S), schema(T)) :-
	nonvar(S),
	extend_schema_effects(S, S_exp),
	expand_schema_contents(S_exp, T).

	
% ----- Helper predicates for term expansion -----
	
% Expand schema contents (the part of the dots in   schema(...)  )
expand_schema_contents([], []).

expand_schema_contents([Elem | List], [ElemMod | ListMod]) :-
	expand_schema_element(Elem, ElemMod)
,	expand_schema_contents(List, ListMod)
.
	
% Expand type
expand_schema_element(type(T), type(T_exp)) :-
	nonvar(T)
,	rdf_db:rdf_global_id(T, T_exp).
	
% Expand arguments
expand_schema_element(arguments(T), arguments(T_exp)) :-
	nonvar(T)
,	expand_schema_arguments(T, T_exp).	
		
% Expand preconditions
expand_schema_element(preconditions(T), preconditions(T_exp)) :-
	nonvar(T)
,	expand_schema_conditions(T, T_exp).

% Expand effects	
expand_schema_element(effects(T), effects(T_exp)) :-
	nonvar(T)
,	expand_schema_conditions(T, T_exp).
		
% Expand success conditions
expand_schema_element(success_conditions(T), success_conditions(T_exp)) :-
	nonvar(T)
,	expand_schema_conditions(T, T_exp).
	
% Expand failure conditions
expand_schema_element(failure_conditions(T), failure_conditions(T_exp)) :-
	nonvar(T)
,	expand_schema_conditions(T, T_exp).
	
% Expand settings
expand_schema_element(settings(T), settings(T_exp)) :-
	nonvar(T)
,	expand_schema_conditions(T, T_exp).	

% Expand settings
expand_schema_element(location(T), location(T_exp)) :-
	nonvar(T)
,	rdf_db:rdf_global_id(T, T_exp).	

expand_schema_element(characters(T), characters(T_exp)) :-
	nonvar(T)
,	expand_characters(T, T_exp)
.

% Last resort: leave intact
expand_schema_element(X, X).	
	
% --- Contents of elements ---

expand_schema_arguments([], []).

% NOTE: the operator =.. unifies predicates with lists. For example, the following holds:
% 			agens(linda) =.. [agens, linda]
expand_schema_arguments([T1|T], [T1_exp|T_exp]) :-
	expand_schema_argument(T1, T1_exp)
,	expand_schema_arguments(T, T_exp)
.		

expand_schema_argument(T, T_exp) :-
	nonvar(T)
,	T =.. [Type, Arg]
,	rdf_db:rdf_global_id(Arg, Arg_exp)
,	T_exp =.. [Type, Arg_exp]
.

expand_schema_argument(T, T) :-
	var(T).

	
% Expand conditions in general
expand_schema_conditions([], []).

expand_schema_conditions([T1|T], [T1_exp|T_exp]) :-
	expand_condition(T1, T1_exp)
,	expand_schema_conditions(T, T_exp)
.
	
% Expand one condition
expand_condition(condition(Truth, []), condition(Truth, [])).

expand_condition(condition(Truth, [E1 | E]), condition(Truth, [E1_exp | E_exp])) :-
	expand_condition_member(E1, E1_exp)
,	expand_condition_memberlist(E, E_exp)
.
	
expand_condition(condition(Truth, Elem), condition(Truth, Elem_exp)) :-
	expand_condition_member(Elem, Elem_exp).
	
expand_condition(C, C) :-
	var(C).	
	
% Expand the list of members of a condition
expand_condition_memberlist([], []).

expand_condition_memberlist([L1|L], [L1_exp|L_exp]) :-
	expand_condition_member(L1, L1_exp)
,	expand_condition_memberlist(L, L_exp)
.	
	
% Expand a member of a condition
% NOTE: the operator =.. unifies predicates with lists. For example, the following holds:
% 			fact(linda, has, ice) =.. [fact, linda, has, ice]
expand_condition_member(Elem, Elem_exp) :-
	nonvar(Elem)
,	Elem =.. [Type | Args]
,	expand_args(Args, Args_exp)
,	Elem_exp =.. [Type | Args_exp]
.

expand_condition_member(Elem, Elem) :-
	var(Elem).
	
% Expand arguments of a fact. 
expand_args([], []).

expand_args([Arg1 | Args], [Arg1_exp | Args_exp]) :-	
	rdf_db:rdf_global_id(Arg1, Arg1_exp)
,	expand_args(Args, Args_exp)
.

expand_args(A, A) :-
	var(A).
	
expand_characters([], []).

expand_characters([H|T], [H_exp|T_exp]) :-
	expand_character(H, H_exp)
,	expand_characters(T, T_exp)
.

expand_character(character(T), character(T_exp)) :-
	nonvar(T)
,	rdf_db:rdf_global_id(T, T_exp)
.

expand_character(character(T), character(T)) :-
	var(T).

% This adds a KR of the schema itself to the effects of the schema.	
extend_schema_effects(S, T) :-
	memberchk(effects(Effs), S) % Only do this for operators
,	!
,	make_extended_effects(S, Effs, Effs2)
,	replace(S, effects(Effs), effects(Effs2), T).

% Leave non-operators intact	
extend_schema_effects(S, S).

% Makes effects that are extended with fabula describing the schema itself.
% E.g. an action schema "Buy" would be extended with a condition containing
% statements like fabula(operator234, rdf:type, lolli:Buy)
make_extended_effects(S, Effs, Effs2) :-
	gensym(operator_dummy, Name) 
	% Add type
,	memberchk(type(Type), S)
,	TypeFabula = [fabula(Name, rdf:type, Type)]
,	memberchk(arguments(Args), S)
,	!	% Cut; now we must continue extending rather than keep identical.
,	ignore((
		memberchk(agens(A), Args), 
		AgensFabula = [fabula(Name, fabula:agens, A)], 
		CharacterFabula = [fabula(Name, fabula:character, A)] % Note: assumption is that agens of the operator
															  % is also the character that 'has' the operator
	))
,	ignore((memberchk(patiens(P), Args), PatiensFabula = [fabula(Name, fabula:patiens, P)]))
,	ignore((memberchk(target(T), Args), TargetFabula = [fabula(Name, fabula:target, T)]))
,	ignore((memberchk(instrument(I), Args), InstrumentFabula = [fabula(Name, fabula:instrument, I)]))
,	append_if_nonvar([], TypeFabula, FabulaList1)
,	append_if_nonvar(FabulaList1, AgensFabula, FabulaList2)
,	append_if_nonvar(FabulaList2, PatiensFabula, FabulaList3)
,	append_if_nonvar(FabulaList3, TargetFabula, FabulaList4)
,	append_if_nonvar(FabulaList4, InstrumentFabula, FabulaList5)
,	append_if_nonvar(FabulaList5, CharacterFabula, FabulaList6)
,	append_if_nonvar(Effs, [condition(true, FabulaList6)], Effs2)
.

make_extended_effects(_, Effs, Effs).

% Replace item in empty list: returns empty list
replace([], _, _, []).

% Replace found element
replace([Elem1|List1], Elem1, Elem2, [Elem2|List2]) :-
	!,
	replace(List1, Elem1, Elem2, List2).
	
% Continue if not found	
replace([H|List1], Elem1, Elem2, [H|List2]) :-
	replace(List1, Elem1, Elem2, List2).
	
% Append, only if lists are no variables.
append_if_nonvar(List1, List2, _) :-
	var(List1)
,	var(List2)
,	!
.

append_if_nonvar(List1, List2, List1) :-
    var(List2)
,	!
.

append_if_nonvar(List1, List2, List2) :-
    var(List1)
,	!
.    
    
append_if_nonvar(List1, List2, List3) :-
	append(List1, List2, List3).

% --------------------------------------------------------
% Schema access predicates
% Note: each access predicate that handles data with URIs
% 		(e.g. the type of a schema is an RDF URI)
%		is accompanied by a user:goal_expansion predicate.
%		What a goal expansion predicate does, is translate
%		each goal in any of the Prolog files into another
%		goal _at compile time_. For example, if any Prolog
%		file contains a predicate containing a goal 
%		schema_agens(lolli:linda), this is replaced by a goal
%		schema_agens('http://....#linda') when the file is
%		compiled. Advantage is MAJOR speedup and robustness.
% --------------------------------------------------------

% Get type of schema
schema_type(S, T) :-
    schema(S)
,   memberchk(type(T), S).

	% Goal expansion, so that predicates can call the goal with prefixed URIs.
	user:goal_expansion(schema_type(S, T), schema_type(S, T2)) :-
		nonvar(T),	% To prevent self-loop with its own definition.
		expand_schema_element(type(T), type(T2)).
	

% Get duration of operator (only operator schemas have durations).
operator_duration(S, D) :-
    operator_schema(S)
,	has_schema_element(S, duration(D), 1)
.

	% No goal expansion: duration does not contain any URIs.
	% .

% Get preconditions of schema
schema_preconditions(S, P) :-
    schema(S)    
,	has_schema_element(S, preconditions(P), [])
.  

	% Goal expansion, so that predicates can call the goal with prefixed URIs.
	user:goal_expansion(schema_preconditions(S,P), schema_preconditions(S, P2)) :-
		nonvar(P),
		expand_schema_element(preconditions(P), preconditions(P2)).  
	
	
% Get effects of operator (only operator schemas have effects)
operator_effects(S, E) :-
    operator_schema(S)
,	has_schema_element(S, effects(E), [])
.    	

	% Goal expansion, so that predicates can call the goal with prefixed URIs.
	user:goal_expansion(operator_effects(S,E), operator_effects(S, E2)) :-
		nonvar(E),
		expand_schema_element(effects(E), effects(E2)).  
		
% Get effects of operator as triples (deprecate?)		
operator_effect(Schema, Truth, S, P, O) :-
	operator_effects(Schema, Effs)					% -> e.g. [ condition(..), condition(..), ...]
,	member(Eff, Effs)									% -> e.g. condition(true, [...])
,	condition_member(EffMember, Eff)			% -> e.g. condition(true, fact(...))
,	condition_to_triple(EffMember, Truth, S, P, O)
.

% Helper function to translate condition to triple
% TODO: make fabula(S,P,O,G) variant
condition_to_triple(Condition, Truth, S, P, O) :-
	Condition = condition(Truth, Fact),
	Fact = fact(S, P, O)
%	;	Fact = fabula(S, P, O)       -> this does not work correctly; it will produce (operator_dummy23 rdf:type lolli:'AskFor') stuff.
	.

% Determines whether schema contains given element, or uses the default value if doesn't contain any information on the element.
has_schema_element(Schema, Element, DefaultValue) :-
	nonvar(Schema)
,	nonvar(Element)
,	Element =.. [Type, Value]   	% e.g.,  [duration, 1]
,	Element_templ =.. [Type, Temp]	% e.g.,  [duration, X]
,	(	memberchk(Element_templ, Schema) 
	->  Value = Temp
	;	Value = DefaultValue
	)
.

% Predicate for setting and checking default values.
default(Var, Val) :-
	var(Var)
,	nonvar(Val)
,	Var = Val
.

% Get success conditions of goal (only goal schemas have success conditions)
goal_success_conditions(S, C) :-
    goal_schema(S)
,	has_schema_element(S, success_conditions(C), [])
.    

	% Goal expansion, so that predicates can call the goal with prefixed URIs.
	user:goal_expansion(goal_success_conditions(S,E), goal_success_conditions(S, E2)) :-
		nonvar(E),
		expand_schema_element(success_conditions(E), success_conditions(E2)).


% Get failure conditions of goal (only goal schemas have failure conditions)	
goal_failure_conditions(S, C) :-
    goal_schema(S)
,	has_schema_element(S, failure_conditions(C), [])
.

	% Goal expansion, so that predicates can call the goal with prefixed URIs.
	user:goal_expansion(goal_failure_conditions(S,E), goal_failure_conditions(S, E2)) :-
		nonvar(E),
		expand_schema_element(failure_conditions(E), failure_conditions(E2)).

goal_urgency(S, C) :-
	goal_schema(S)
,	has_schema_element(S, urgency(C), 0.5)	
.

action_selection(S, A) :-
	action_selection_schema(S)
,	has_schema_element(S, selected_action(A), [])
.
	
	
% Get scope of framing schema
framing_scope(S, Scope) :-
	framing_schema(S)
,	has_schema_element(S, scope(Scope), all)	
.

framing_scope_all(S) :-
	framing_scope(S, all).
	
framing_scope_personal(S) :-
	framing_scope(S, personal).
	
framing_scope_hidden(S) :-
	framing_scope(S, hidden).		

% Get arguments of schema: agens, patiens etc.
schema_arguments(S, A) :-
    schema(S)
,	has_schema_element(S, arguments(A), [])    
.    

	% Goal expansion, so that predicates can call the goal with prefixed URIs.
	user:goal_expansion(schema_arguments(S,A), schema_arguments(S, A2)) :-
		nonvar(A),
		expand_schema_element(arguments(A), arguments(A2)).


% Get agens of schema
schema_agens(S, X) :-
    schema_arguments(S, A)
,	memberchk(agens(X), A)
.
	
	% Goal expansion, so that predicates can call the goal with prefixed URIs.
	user:goal_expansion(schema_agens(S, A), schema_agens(S, A2)) :-
		nonvar(A),
		expand_schema_arguments([agens(A)], [agens(A2)]).	
		
	
% Get patiens of schema
schema_patiens(S, X) :-
    schema_arguments(S, A),    
	memberchk(patiens(X), A).

	% Goal expansion, so that predicates can call the goal with prefixed URIs.
	user:goal_expansion(schema_patiens(S,A), schema_patiens(S, A2)) :-
		nonvar(A),
		expand_schema_arguments([patiens(A)], [patiens(A2)]).		
		
	
% Get target of schema
schema_target(S, X) :-
    schema_arguments(S, A),
	memberchk(target(X), A).
	
	% Goal expansion, so that predicates can call the goal with prefixed URIs.
	user:goal_expansion(schema_target(S,A), schema_target(S, A2)) :-
		nonvar(A),
		expand_schema_arguments([target(A)], [target(A2)]).			
		
	
% Get instrument of schema	
schema_instrument(S, X) :-
    schema_arguments(S, A),
	memberchk(instrument(X), A).
	
	% Goal expansion, so that predicates can call the goal with prefixed URIs.
	user:goal_expansion(schema_instrument(S,A), schema_instrument(S, A2)) :-
		nonvar(A),
		expand_schema_arguments([instrument(A)], [instrument(A2)]).		

% Get opponent of schema	
schema_opponent(S, X) :-
    schema_arguments(S, A),
	memberchk(opponent(X), A).
	
	% Goal expansion, so that predicates can call the goal with prefixed URIs.
	user:goal_expansion(schema_opponent(S,A), schema_opponent(S, A2)) :-
		nonvar(A),
		expand_schema_arguments([opponent(A)], [opponent(A2)]).		

% Add access predicates and goal expansion for other relevant arguments here.
% ...


% -----------------------------------------------------------------
% Validation of schemas, given current state of the knowledge base
% validate_schema(+Schema, -FalsePreconditionList)
%
% This functions needs an operator as an input, and returns a
% list with preconditions that do not hold.
%
% Notice that validate_schema(+Schema, []) is true iff the operator
% is valid.
% -----------------------------------------------------------------

% Validate a schema
validate_schema(S, UP):-
	nonvar(S)
,	schema_preconditions(S, P)
,   validate_conditions(P, UP)
.

% Validate the failure conditions of given goal schema
validate_goal_failure_conditions(S) :-
	goal_schema(S)
,	goal_failure_conditions(S, FC)
,	FC \= []	% Avoids the case that an empty list of failure conditions validates.
,	validate_conditions(FC, [])
.

% Validate the failure conditions of given goal schema
validate_goal_success_conditions(S) :-
	goal_schema(S)
,	goal_success_conditions(S, SC)
,	validate_conditions(SC, [])
.
    
% Validate (pre)conditions: empty list is valid.
validate_conditions([], []).

% Validate conditions that must be TRUE
validate_conditions([First | Rest], UP) :-
    validate_condition(First)
%,	! % Cut; look on. Disabled to allow alternative ways for the validation to succeed (for false conditions).
,   validate_conditions(Rest, UP)
.

% Add condition to list of untrue preconditions if doesn't match above predicate.
validate_conditions([First | Rest], [First | UP]) :-
	\+ validate_condition(First)
,	validate_conditions(Rest, UP).

% Validate a condition
validate_condition(condition(true, [])) :-
	! % We are done if the true condition contains no facts.
.	
	
% Validate a condition
validate_condition(condition(false, [])) :-
	!
,	fail % We fail if the false condition contains no facts.
.		

% A true condition validates, if we can validate all its members.
validate_condition(condition(true, [L1|Rest])) :-
	validate_condition(condition(true, L1)),
	validate_condition(condition(true, Rest)).
	
% A true condition is valid(ated) if querying it is successful
validate_condition(condition(true, Fact)) :-
	\+ is_list(Fact)	% Make sure this predicate is not used for lists of facts
,   query(Fact).

% A false condition list validates, if we cannot validate its true variant:		not(L1 /\ .. /\ Ln)
validate_condition(condition(false, Contents)) :-
	\+ validate_condition(condition(true, Contents)).

% A true condition is valid(ated) if querying it is unsuccessful    
%validate_condition(condition(false, Fact)) :-
%	\+ is_list(Fact)	% Make sure this predicate is not used for lists of facts
%,   unpQuery(Fact).    
    
% Given a condition predicate, condition_member chooses one of its subconditions
% being a member of the condition.
% E.g. a condition:   condition(true, [fact(a,b,c), fact(d,e,f)])
% gives the following results as a condition member:
%					1) condition(true, fact(a,b,c))
%					2) condition(true, fact(d,e,f))
%
% Used by e.g. the planner, but usable by any algorithm that is seeking to fulfill
% a condition.
%
% (1) First look at rules (since they cannot be planned for, they are most "urgent" members)
condition_member(CondMember, Cond) :-
    Cond = condition(Truth, List)
,   member(L1, List)
,	L1 = rule(_,_,_)
,   CondMember = condition(Truth, L1)
.    
% (2) Look at the non-rules next
condition_member(CondMember, Cond) :-
    Cond = condition(Truth, List)
,   member(L1, List)
,	L1 \= rule(_,_,_)
,   CondMember = condition(Truth, L1)
.   

% Version that doesn't bind but retrieve bindings (for continuous planner in development; disabled for now)
condition_member(CondMember, Cond, Bindings) :-
	fail
,	Cond = condition(Truth, List)
,	member(L1, List)
,	CondMember = condition(Truth, L2)
,	unifiable(L1, L2, Bindings)
.
    

% -----------------------------------------------------------------
% apply_operator_effects(+Schema)
%
% applies the effects of an operator.
%
% WARNING: No check whatsoever is made if the operator is valid
% -----------------------------------------------------------------

% We can apply operator effects
apply_operator_effects(S) :-
	% if it has them
	operator_effects(S, Es)
,	!
	% and we can apply every effect
,	apply_operator_effects1(Es).

apply_operator_effects1([E1 | E]) :-
	!
,	apply_effect(E1)
,	apply_operator_effects1(E)
.

apply_operator_effects1([]) :-
	!
.

	
% Apply positive effect: assert	only facts
apply_effect(condition(true, [L1 | List])) :-
	L1 = fact(_, _, _)
,	!	% Cut to prevent concurrent operation on RDF DB
,	knowledgebase:assert_knowledge(L1)
,	apply_effect(condition(true, List))
.
    
% Apply negative effect: retract only facts
% For the pure semantics of applying negative conditions, it would be enough to choose one of its members
% that is currently asserted, and retract it. However, to avoid nondeterminism, the practical semantics 
% of applying a negative condition is to apply ALL its members.
apply_effect(condition(false, [L1 | List])) :-
	L1 = fact(_, _, _)
,	!	% Cut to prevent concurrent operation on RDF DB
,	knowledgebase:retract_knowledge(L1)
,	apply_effect(condition(false, List))
.
    
% If we get to this point, apply_effect will not fail but just do nothing.    
apply_effect(_) :-
	!.