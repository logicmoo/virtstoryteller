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

%%%
% Basic World Agent Prolog functions
% @author Ivo Swartjes
% 11-3-2008

:- module(basicWorldAgent,
	[ 
		check_schema_facts/1,	% +Schema
		agent/1
	]).

agent(world).
agent(plot).
agent(character).
	
% Check whether the facts of given (instantiated!) schema hold. Ignores fabula preconditions.
check_schema_facts(S) :-
	nonvar(S)
,	schema_preconditions(S, Prec)
,	validate_conditions_ignore_fabula(Prec)
.

% Validate (pre)conditions: empty list is valid.
validate_conditions_ignore_fabula([]).

% Validate conditions that must be TRUE
validate_conditions_ignore_fabula([First | Rest]) :-
    validate_condition_ignore_fabula(First)
%,	! % Cut; look on. Disabled to allow alternatives.
,   validate_conditions_ignore_fabula(Rest)
.

% Validate a condition
validate_condition_ignore_fabula(condition(true, [])) :-
	! % We are done if the true condition contains no facts.
.	
	
% Validate a condition
validate_condition_ignore_fabula(condition(false, [])) :-
	!
,	fail % We fail if the false condition contains no facts.
.		

% A true condition validates, if we can validate all its members.
validate_condition_ignore_fabula(condition(true, [L1|Rest])) :-
	validate_condition_ignore_fabula(condition(true, L1)),
	validate_condition_ignore_fabula(condition(true, Rest)).
	
validate_condition_ignore_fabula(condition(false, [L1|Rest])) :-
	validate_condition_ignore_fabula(condition(false, L1))
%,	! % Found, we are done! Disabled to allow alternative validations
;	validate_condition_ignore_fabula(condition(false, Rest)).

% A condition is valid(ated) if it is fabula; we ignore it.
validate_condition_ignore_fabula(condition(_, Fact)) :-
	\+ is_list(Fact)
,	Fact =.. [fabula | _]
.

% A true condition is valid(ated) if querying it is successful
validate_condition_ignore_fabula(condition(true, Fact)) :-
	\+ is_list(Fact)	% Make sure this predicate is not used for lists of facts
,   query(Fact).

% A true condition is valid(ated) if querying it is unsuccessful    
validate_condition_ignore_fabula(condition(false, Fact)) :-
	\+ is_list(Fact)	% Make sure this predicate is not used for lists of facts
,   unpQuery(Fact). 
