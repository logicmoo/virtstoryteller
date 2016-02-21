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

:- module(goal_manager,
	[	
		possible_goal/2,			% (+Agens, -Goal) 			See which goals are possible in the current context of the simulation
		possible_goal_after_plan/3,	% (+Agens, -Goal, -Plan)	See if we can justify a goal using a plan.
		adopt_goal/1,				% (+Goal) 					Adopt goal.
		drop_goal/1,				% (+Goal)					Drop a goal.
		adopt_justifiable_goal/1,	% (+Goal)					Adopt a goal for justification.
		adopted_goal/1,				% (?Goal)					See which goal is adopted.
		adopted_justifiable_goal/1, % (?Goal)					See which goal is adopted for justification.
		suggest_goal/1,				% (+Goal)	Assert given goal as a suggested goal.
		suggested_goal/1			% (?Goal)	True if Goal was suggested.
	]).
	
:- dynamic adopted_goal/1,
			adopted_justifiable_goal/1,
			suggested_goal/1.

:- use_module(basicCharacterAgent).

	% RDF meta predicates, for term expansion at compile time
:- rdf_meta 
		possible_goal(r, -),
		possible_goal_after_plan(r, -, -),		
		adopt_goal(t),
		drop_goal(t),
		adopt_justifiable_goal(t),
		suggested_goal(t).
		
% ----------------------------------------------------------------------		

% Sees which goals can be adopted. Since schema validations might result in duplicate schemas (if there are multiple ways to validate a schema),
% we generate the whole set and select a member. Assumption here is that the set of goals and the number of calls to this predicate small enough 
% that it remains quick. I estimate that might become a problem with > 1000 goals.
%
% Common usage: 
%	possible_goal(+Agens, -Goal)
%
possible_goal(Agens, Goal) :-
	
	% Generate set of goals that can possibly be adopted (comparison: ==)
	setof(
		G
	,	(
			goal_schema(G)
		,	schema_agens(G, Agens)
		,	validate_schema(G, [])
		,	passes_adoption_filter(G)
		,	\+ validate_goal_success_conditions(G)   % Not already achieved
		,	\+ validate_goal_failure_conditions(G)   % Not already failed		
		)
	,	Set
	)
	% Remove duplicates	(comparison: =)
,	filter_duplicates(Set, Set2)
,	member(Goal, Set2)
.

% Retrieves which goals are possible for given agent, if Plan were executed
% CAREFUL: this predicate might be very slow, depending on domain definitions
% Eventually we might want a predicate that allows for specifying plan depth
% as well, so you can iterate over possible plannable goals.
%
% Common usage: 
%	possible_goal_after_plan(+Agens, -Goal, -Plan)	- To get the goals & a plan that would justify it
%	possible_goal_after_plan(+Agens, +Goal, -Plan)	- To get a plan that justifies given goal for an agent
%
possible_goal_after_plan(Agens, Goal, Plan) :-
	goal_schema(Goal)
,	schema_agens(Goal, Agens)
	% The goal should not be adopted already
,	\+ adopted_goal(Goal)
	% We should be able to make the schema possible (which fails if it already IS possible)
,	make_schema_possible(Goal, Plan)

	% See what is necessary for justification
	% The goal, with its variables bound according to the justification, should not already have been adopted for justification.
,	\+ adopted_justifiable_goal(Goal)

	/*  The below check doesn't work, since the success/failure condition truth values might change due to execution of the justifying plan.
		At this point, we don't know whether the goal will be very useful unless we have executed the plan.
		The assumption is that the goal _will_ be useful, because we have to work to make the preconditions true, it will probably not be immediately
		successful or failed. If it turns out to be, then it will just not be accepted as a goal and we have done superfluous work.
	
,	\+ validate_goal_success_conditions(Goal)   % Not already achieved
,	\+ validate_goal_failure_conditions(Goal)   % Not already failed
	
	*/

.

% Adopts new (given) goal
%	
% Common usage:
%	adopt_goal(+Goal)
%
adopt_goal(Goal) :-
	% Not already adopted
	\+ adopted_goal(Goal)
	% Preconditions still hold
,	validate_schema(Goal, [])
	% OK, adopt.
,	assert(adopted_goal(Goal))
.

% Drops a given goal
drop_goal(Goal) :-
	nonvar(Goal)
,	retract(adopted_goal(Goal))
.

% Adopt goal to justify
adopt_justifiable_goal(Goal) :-
	\+ adopted_justifiable_goal(Goal)
,	assert(adopted_justifiable_goal(Goal))
.

% Take on given goal as suggested
suggest_goal(Goal) :-
	nonvar(Goal)
,	assert(suggested_goal(Goal))
.

% -------- INTERNAL PREDICATES (not exported by module) ----------

% Try to make a plan that will enable the preconditions of a schema
% Fails if there are no untrue conditions: we cannot MAKE schema possible; it already is.
make_schema_possible(Schema, Plan) :-
	validate_schema(Schema, UntrueConditions)
,	UntrueConditions \= []
,	plan(_, UntrueConditions, Plan).   

% The empty set has no duplicates
filter_duplicates([], []).

% If the head of the set is a member (=) of the rest, then the result is the filtered tail 
filter_duplicates([S1|S], T) :-
	member(S1, S)
,	filter_duplicates(S, T)
.

% If the head of the set is NOT a member (=) of the rest, then the result is the filtered tail plus the head.
filter_duplicates([S1|S], [S1|T]) :-
	\+ member(S1, S)
,	filter_duplicates(S,T)
.


% A goal schema passes the adoption filter
% This will be the place to do coherence-based constraints, such as specify that the goal must be caused by something in the causal chain.
passes_adoption_filter(S) :-
	% (1) it is not yet adopted already
	\+ adopted_goal(S)
.