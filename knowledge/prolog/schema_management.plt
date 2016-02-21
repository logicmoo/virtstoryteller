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

:- begin_tests(schema_management).
:- use_module('schema_management.pl').

% See whether there are possible bindings for the preconditions of every operator,
% either by another operator or by the story world
test(pop_all_operators_are_possible, [nondet, blocked('this is almost never true')]) :-
    \+ impossible_operator(_S, _PP).


% We should not be allowed to retrieve a schema by partially describing it.
test(partial_schema, [fail]) :-
    schema([
	    type(_)
		])
	.
	
% But we can retrieve it by using subset.	
test(partialschema2, [nondet]) :-
    schema(S),
    subset([ type(_) ], S).
    
test(test_schema1, [
		setup( consult(schemas:'../domain/test/schemas/schema.pl')  ),
        nondet]) :-
	schema(S),
	schema_type(S, test),
	schema_preconditions(S, Precs),
	member(Prec, Precs),
	condition_member(_, Prec).
	
% Tests whether all variables declared in the effects of operators also occur in the preconditions; this is a requirement for plan completion
% of partial order planning. Only ignore if you know what you're doing.
test(test_vars_in_effects_of_operator_are_all_defined_in_preconditions, [
        setup( nb_setval(failed, no) ),
        cleanup( nb_delete(failed)  ),
        nondet]) :-
    forall(
        operator_schema(S)
    ,
        (   schema_preconditions(S, PC)
        ,   term_variables(PC, PCvars)
        ,   operator_effects(S, EFF)
        ,   term_variables(EFF, EFFvars)
        ,   variable_superset(PCvars, EFFvars, Vars)         
        ,   (   
                Vars = []
            )
            ;   
            (   schema_type(S, T)
            ,   format('    Schema has variables in effects that are not in preconditions: ~p~n',[T])                
            ,   nb_setval(failed, yes)
            )   
        )
    )
,   nb_getval(failed, no)
.

% Tests whether all conditions of schemas are of one kind only, by searching for one for which it is not the case.
test(test_preconditions_are_not_one_kind, [nondet, fail]) :-
    schema(S)
,   schema_preconditions(S, PC)
,   conditions_contain_mixed_kinds(PC)
,   schema_type(S, ST)
,   format('    Preconditions of schema have mixed kinds: ~p~n', [ST])
.

% Tests whether all conditions of schemas are of one kind only, by searching for one for which it is not the case.
test(test_effects_are_not_one_kind, [nondet, fail]) :-
    operator_schema(S)
,   operator_effects(S, Eff)
,   conditions_contain_mixed_kinds(Eff)
,   schema_type(S, ST)
,   format('    Effects of operator schema have mixed kinds: ~p~n', [ST])
.

% Tests whether all conditions of schemas are of one kind only, by searching for one for which it is not the case.
test(test_success_conditions_are_not_one_kind, [nondet, fail]) :-
    goal_schema(S)
,   goal_success_conditions(S, SC)
,   conditions_contain_mixed_kinds(SC)
,   schema_type(S, ST)
,   format('    Effects of operator schema have mixed kinds: ~p~n', [ST])
.

% Tests whether all conditions of schemas are of one kind only, by searching for one for which it is not the case.
test(test_failure_conditions_are_not_one_kind, [nondet, fail]) :-
    goal_schema(S)
,   goal_failure_conditions(S, FC)
,   conditions_contain_mixed_kinds(FC)
,   schema_type(S, ST)
,   format('    Effects of operator schema have mixed kinds: ~p~n', [ST])
.

% Tests schema element getting
test(test_schema_elements_1, [nondet]) :-
    schemas:has_schema_element([name(ivo)], name(ivo), ivo).
    
% ...using a variable
test(test_schema_elements_2, [nondet]) :-
    schemas:schemas:has_schema_element([name(ivo)], name(X), ivo), X = ivo.    
     
% ...using a variable with different default
test(test_schema_elements_3, [nondet]) :-
    schemas:has_schema_element([name(ivo)], name(X), jan), X = ivo.      

% ...for nonexisting element
test(test_schema_elements_4, [nondet]) :-
    schemas:has_schema_element([name(ivo)], age(28), 28).    
    
% ...using a variable for nonexisting element    
test(test_schema_elements_5, [nondet]) :-
    schemas:has_schema_element([name(ivo)], age(X), 28), X = 28.    
    
% Tests schema element getting (fail)
test(test_schema_elements_1, [nondet, fail]) :-
    schemas:has_schema_element([name(ivo)], name(jan), ivo).
    
% ...trying to grab default anyway
test(test_schema_elements_2, [nondet, fail]) :-
    schemas:has_schema_element([name(ivo)], name(X), jan), X = jan.    

% Contains mixed kinds conditions
conditions_contain_mixed_kinds(ConditionList) :-
    member(P1, ConditionList)
,   P1 = condition(_, P1List)
,   member(P1m1, P1List)
,   member(P1m2, P1List)
,   P1m1 \== P1m2
,   P1m1 =.. [Type | _]
,   P1m2 =.. [Type2 | _]
,   Type \== Type2
.

% variable_superset/3
% variable_superset(+A, +B, ?C)     given two lists of variables A and B, unifies C with the variables that are in B but not in A. 
variable_superset([], V2, Vars) :-
    term_variables(V2, Vars).

variable_superset([H|T], V2, Vars) :-  
    var(H),
    H = dummy,
    variable_superset(T, V2, Vars).    

:- end_tests(schema_management).