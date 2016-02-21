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

% Een episode heeft precondities, en definieert setting information, die variabelen kan bevatten.

:- module(episodes,
	[
		possible_thread/1,		% - thread-descriptor	queries which plot threads are possible
%		start_episode/1,		% + episode-descriptor	starts a certain plot thread
		necessary_character/2,	% + thread schema - characterURI		queries which characters are necessary in given thread
%		casted_character/1,		% + characterURI		asserts which characters have been casted
		thread_goal/3,	    	% + thread schema + characterURI, -thread goals	queries the thread goals for given character
		thread_resolve_goal/3,	% + thread schema + characterURI, -thread goals	queries the thread resolve goals for given character
		thread_setting/2,    	% + thread schema - settings for plot thread
		condition_to_triples/5	% + thread schema, - Truth -Subj -Pred -Obj
	]).
	
:- use_module(schema_management).
:- use_module(planner).
	
:- dynamic
    character/1,
    goal/2,
    setting/1.

    
% necessary_character/1 - characterURI
necessary_character(S, C) :-
	thread_schema(S),
	memberchk(characters(Chars), S),
	member(character(C), Chars).
	
% Retrieve episodic goal for character
thread_goal(S, Char, Goal) :-
	thread_schema(S),
	memberchk(goals(Goals), S),	
	member(goal(Char, Goal), Goals).

% Retrieve resolve goals for character
thread_resolve_goal(S, Char, Goal) :-
	thread_schema(S),
	memberchk(resolve_goals(Goals), S),	
	member(goal(Char, Goal), Goals).

	
% Retrieve episodic setting for character
% For now, only works for facts
thread_setting(S, Cond) :-
	thread_schema(S),
    memberchk(settings(Settings), S),
    member(Cond, Settings).
        
condition_to_triples(Cond, Truth, S, P, O) :-
    condition_member(Cond1, Cond),
    Cond1 = fact(Truth, S, P, O).



% casted_character/1  +character URI
% Idea is that Java queries cast_character(X), resulting in e.g. 'ps:leChuck', 
% and after querying, this character is retracted as a character to be casted
%casted_character(S, CX) :-
%    character(X),
%    retract(character(X)).

% ---------------- episode schemas and their preconditions ----------------    

% Thread is possible if its preconditions are valid. Do not search for a plan.
% Included this version of the predicate because of a bug in the planner (see achieved_subcondition/4 in pop.pl)
% Added bonus is a speedup.
% -- Ivo, 1 juli 08
possible_thread(S) :- 
	thread_schema(S)
,	validate_schema(S, [])
,	!
.

% Thread is possible (if its preconditions are not valid and) a plan can be found to validate them.
possible_thread(S) :- 
	thread_schema(S)
,	nonvar(S)
,	schema_preconditions(S, P)
,	!
,	plan(_,P,_)
.
