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

% Module knowledgebase
%
% Initializes the OWL reasoning modules,
% and defines a query, assert and retract interface on top of it.

:- module(knowledgebase, [    
	query/1 							% ?(Subject, Predicate, Object)
,	query/3 							% ?Subject, ?Predicate, ?Object
,	unpQuery/1							% ?(Subject, Predicate, Object)
,	assert_knowledge/1                  % +Knowledge
,	retract_knowledge/1                 % +Knowledge
,    rdfAssert/1                         % +(Subject, Predicate, Object)
%,    rdfAssert/2                         % +(Subject, Predicate, Object), +GraphName
,   rdfRetract/1                        % +(Subject, Predicate, Object)
%,	expandPrefix/2                      % +Facts, -ExpandedFacts
,   getSubject/2                        % +(Subject, Predicate, Object), -Subject
,   getPredicate/2                      % +(Subject, Predicate, Object), -Predicate
,   getObject/2                         % +(Subject, Predicate, Object), -Object
,   first/2                             % +(A, B), -A
,   second/2                            % +(A, B), -A
]).

% Set paths for the various files.
:- multifile
	user:file_search_path/2,
	rule/3.
:- dynamic
	user:file_search_path/2.
:- dynamic
	rdf_quad/4.
	
user:file_search_path(semweb, library(semweb)).

% Load OWL/RDF/RDFS reasoning.
%:- use_module(rdf_base).
:- use_module(library(semweb/rdf_db)).
:- use_module('owl.pl').
:- use_module(library(semweb/rdf_turtle)).


% RDF meta predicates, for term expansion at compile time
:- rdf_meta 
	query(t),
	unpQuery(t),
	query(r, r, o),
	rule(r, r, o),
	fact(r, r, o),
	fabula(r, r, o),
	fabula(r, r, o, r),
	rdfAssert(t),
	rdfRetract(t),
	assert_knowledge(:),
	retract_knowledge(:).

% Load in rules
:- rdf_db:rdf_register_ns(swcr, 'http://www.owl-ontologies.com/SWCRules.owl#').
:- rdf_db:rdf_register_ns(owlr, 'http://www.owl-ontologies.com/OWLRules.owl#').
:- include('owl_rules.pl').
:- include('swc_rules.pl').

% Assert a clause which does nothing. This is necessary to make the user
% database editable (enable retract and assert on the database).
%
% EDIT: Not necessary anymore because of the owl:'Transitive' hack on rdfs:subClassOf.
%
%:- rdf_edit:rdfe_transaction(rdfe_assert(a,b,c)).

% Make rdfs:subClassOf an instance of owl:'Transitive'. This is necessary because
% the semantic web package does not yet really support the transitivity of rdfs:subClassOf
%:- rdf_edit:rdfe_transaction(rdfe_assert(rdfs:subClassOf, rdf:type, owl:'TransitiveProperty')).
:- rdf_assert(rdfs:subClassOf, rdf:type, owl:'TransitiveProperty').


%-----------------------------------------------------
% query/1 and query/3
% Query(+SinglePreconditionClause)
%
% Queries if a SinglePreconditionClause is true
%-----------------------------------------------------
	
query(agent(A)) :-
	agent(A).
	
% Querying a fact statement.	
query(fact(S,P,O)) :-
	query(S,P,O).


% Querying a fabula statement.	
query(fabula(S,P,O,G)) :-
	query(S,P,O,G).
	
% Variant for when the graph name is not given or is the main graph; just query the triple
query(fabula(S, P, O, G)) :-
	G = 'http://www.owl-ontologies.com/Graphs#maingraph'
,	query(fabula(S, P, O))
.		

% Querying a fabula statement.	
query(fabula(S,P,O)) :-
	query(S,P,O).


% Querying RDF triple (for backwards compatibility).
query((S, P, O)):-
	query(S, P, O). % remove brackets.
		
query(rule(S, P, O)) :-
	rule(S, P, O).		

% Querying RDF quad.	
	
query(S, P, O, G) :-
	rdf_quad(S, P, O, G).
	

% query(+Subject, +Predicate, +Object)
% Query owl database
	
query(S, P, O) :-
%	rdf_db:rdf_global_id(S, S1), % deal with namespaces
%	rdf_db:rdf_global_id(P, P1),
%	rdf_db:rdf_global_id(O, O1),
	owl:owl_has(S, P, O). % pose the query	

% Query the owl database for the non-existence of X
unpQuery(X) :-
	\+query(X).
	

% -----------------------
% assert_knowledge/1.
% Asserting new knowledge
% -----------------------

assert_knowledge(fact(S,P,O)) :-
    rdfAssert((S,P,O)).
    
assert_knowledge(fabula(S,P,O)) :-
    rdfAssert((S,P,O)).
    
assert_knowledge(fabula(S,P,O,G)) :-
    rdfAssert((S,P,O,G)).       
    
% -----------------------
% retract_knowledge/1.
% Retracting knowledge
% -----------------------

retract_knowledge(fact(S,P,O)) :-
    rdfRetract((S,P,O)).
    
retract_knowledge(fabula(S,P,O)) :-
    rdfRetract((S,P,O)).
    
retract_knowledge(fabula(S,P,O,G)) :-
    rdfRetract((S,P,O,G)).     
	
% -----------------------------------
% Asserting low level RDF triple/quad
% -----------------------------------
% TODO: change to fact/fabula, like in query/1, and then get rid of global_id thingies

% Assert triple with source graph G.
rdfAssert((S, P, O, G)):-
	nonvar(S),
	nonvar(P),
	nonvar(O),
	nonvar(G),
	asserta(rdf_quad(S, P, O, G)).

% Assert triple.
rdfAssert((S, P, O)):-
	nonvar(S),
	nonvar(P),
	nonvar(O),
	rdf_db:rdf_assert(S, P, O, 'http://www.owl-ontologies.com/Graphs#maingraph'), !.	

% Assert triple with source graph G.
rdfRetract((S, P, O, G)):-
	nonvar(S),
	nonvar(P),
	nonvar(O),
	nonvar(G),
	retract(rdf_quad(S, P, O, G)).

% Retract triple.
rdfRetract((S, P, O)):-
	rdf_db:rdf_retractall(S, P, O), !.	

% -----------------
% Helper predicates
% -----------------

% Swartjes 28-8-07
getSubject((S, _P, _O), S).
getSubject(rdf(S, P, O, _), S) :- getSubject((S, P, O), S).

getPredicate((_S, P, _O), P).
getPredicate(rdf(S, P, O, _), P) :- getPredicate((S, P, O), P).

getObject((_S, _P, O), O).
getObject(rdf(S, P, O, _), O) :- getObject((S, P, O), O).

first((A, _B), A).
second((_A, B), B).

