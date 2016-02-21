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

% ------------------------------------------
% load.pl
% ------------------------------------------
%
% Sets up the knowledge base and prolog files necessary to have a basic setup for an agent.
% Can also be used independently.
%
% Author: Ivo Swartjes
% Date:   26 nov 2007
% ------------------------------------------

:- use_module(library(semweb/rdf_db)).
:- use_module(library(threadutil)).

% Load ontologies
:- nl,writeln('# Loading domain-independent ontologies...'),nl.

:- rdf_db:rdf_register_ns(swc, 'http://www.owl-ontologies.com/StoryWorldCore.owl#'). 
:- rdf_db:rdf_register_ns(fabula, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#').
:- rdf_db:rdf_load('../ontology/StoryWorldCore.owl').  % Unfortunately, the db(..) option is only in SWIPL 5.6.37+
:- rdf_db:rdf_load('../ontology/FabulaKnowledge.owl').  % Unfortunately, the db(..) option is only in SWIPL 5.6.37+

% Set up knowledge base
:- nl,writeln('# Loading knowledge base...'),nl.
:- use_module('knowledgebase.pl').

% Set up schema control
:- nl,writeln('# Loading schema control...'),nl.
:- use_module('schema_management.pl').
%:- consult('schemas/schema.pl').

% Set up narrator
:- nl,writeln('# Loading narrator (for debugging purposes)...'),nl.
:- use_module('narrator.pl').

% Load global operators
:- consult(schemas:'global_operators.pl').

% Story domain is loaded through GUI

% ------------------
% DEBUG FUNCTIONS
% TODO: move to more sensible place
% ------------------

load_domain(S) :-
    concat_atom(['../domain/', S, '/loadDomain.pl'], '', A),
    consult(A).

% We are done loading the files. If you want to run unit tests, use test/0:
test :-
    load_test_files([]),
    run_tests.

% Retrieve all operators that can under no circumstance be executed, and the precondition that causes that.
impossible_operator(T, PP, none) :-
        
        % is a schema with...
        schema(S),
        (getFromSchema(S, class(action)) ; getFromSchema(S, class(improvisation)) ),
        
        %getFromSchema(S, class(action)),
        getFromSchema(S, posPreconditions(PPs)),
        getFromSchema(S, head(H)),
        getFromHead(H, type(T)),
        member(PP, PPs),
        \+ checkPrecondition(PP, none).
        
impossible_operator(T, none, NP) :-
        % is a schema with...
        schema(S),
        (getFromSchema(S, class(action)) ; getFromSchema(S, class(improvisation)) ),
%        getFromSchema(S, class(action)),
        getFromSchema(S, negPreconditions(NPs)),
        getFromSchema(S, head(H)),
        getFromHead(H, type(T)),
        member(NP, NPs),
        \+ checkPrecondition(none, NP).


checkPrecondition(PP, NP) :-
    chooseStart(PP, NP), ! ;
    chooseSchema(PP, NP), !.
    
chooseStart(PP, none) :-
    query(PP). %, format('start state.~n').
    
chooseStart(none, NP) :-
    unpQuery(NP). %, format('start state.~n').
    
chooseSchema(PP, none) :-
    % choose action
    schema(NewOperator),
    getFromSchema(NewOperator, class(action)),	    
    getFromSchema(NewOperator, posEffects(PEs)),
    expandPrefix(PEs, PEs1),
	expandPrefix(PP, PP1),    
    memberchk(PP1, PEs1).
    %getFromSchema(NewOperator, head(H)),
    %getFromHead(H, type(NewT)). %,
    %format('schema ~p~n', NewT).
    
chooseSchema(none, NP) :-
    % choose action
    schema(NewOperator),
    getFromSchema(NewOperator, class(action)),	    
    getFromSchema(NewOperator, negEffects(NEs)),
    expandPrefix(NEs, NEs1),
	expandPrefix(NP, NP1),        
    memberchk(NP1, NEs1).
    %getFromSchema(NewOperator, head(H)),
    %getFromHead(H, type(NewT)). %,
    %format('schema ~p~n', NewT).