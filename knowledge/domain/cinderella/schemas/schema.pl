% schema database
% @author Ivo Swartjes
% @date 29 oktober 2007

% Build up schema database (in order of expected use count): 
% schema definitions for story domain

% There are schema definitions in below files (in order of expected use count).
:- include('action.pl').
:- include('goal.pl').
:- include('improvisation.pl').
:- include('event.pl').

% For the rest, anything of below form (i.e., any finished step with some preconditions filled in) 
%    is a schema. CAREFUL: somehow this means we can make a plan for any goal by instantiating this predicate smartly.
schema([	
	class(pop),
	head([type(finished)]),
	posPreconditions(_),
	negPreconditions(_)]).	
