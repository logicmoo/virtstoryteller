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

% To test, use the following:
%rdfAssert(('http://dress1','http://www.owl-ontologies.com/FabulaKnowledge.owl#agens','http://cinderella')).
%rdfAssert(('http://cinderella','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#Human')).
%rdfAssert(('http://dress1','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Dress')).


:- module(narrative_inspiration,
	[ %getSuggestions/1,              % +Suggestions
	  getSuggestion/1,                % -Suggestion
	  getSuggestionName/2,            % +Suggestion, -Name
  	  getSuggestionIndividual/2,        % +Suggestion, -Individual
	  getSuggestionType/2,              % +Suggestion, -Type
	  getSuggestionCausers/2,           % +Suggestion, -Causers (list)	    	  
	  getSuggestionBody/2,              % +Suggestion, -Body
	  nodeClass/3,						% +RDFList, +Class, -Node
	  causalityClass/4,					% +RDFList, +Causer, +Caused, -Causality
	  getFabulaCharacter/3,				% +RDFList, +FabulaNode, -CharacterNode
	  getFabulaContents/3,				% +RDFList, +FabulaNode, -ContentsRDFList
	  getFabulaContentTruth/3,			% +RDFList, +FabulaNode, -Truth	  
	  createValidatedAction/3,          % +RDFList, +ActionClass, -Action	  
	  createValidatedEvent/3,			% +RDFList, +EventClass, -Event	  
	  fabulaNode/1,						% -Node
	  fabulaNode/2,						% +RDFList, -Node
 	  fabulaCause/3          % +RDFList, +Node, -Causality
	]).

% Load in knowledge base
%:- include('../knowledgebase.pl').

% Load in action database
%:- include('../operators/actions.pl').go
%:- include('../operators/events.pl').

% Load in suggestion rules
%:- include('suggestions-plopcase.pl').

% Modules
:- use_module(semweb(rdf_db)).

%getSuggestions(Suggestions) :- setof(suggestion(SuggName, Suggestion), suggestion(SuggName, Suggestion), Suggestions).
getSuggestion(suggestion(Name, Individual, Type, Causers, Body)) :- suggestion(Name, Individual, Type, Causers, Body).

getSuggestionName(suggestion(Name, _Individual, _Type, _Causers, _Body), Name).
getSuggestionIndividual(suggestion(_Name, Individual, _Type, _Causers, _Body), Individual).
getSuggestionType(suggestion(_Name, _Individual, Type, _Causers, _Body), Type).
getSuggestionCausers(suggestion(_Name, _Individual, _Type, Causers, _Body), Causers).
getSuggestionBody(suggestion(_Name, _Individual, _Type, _Causers, Body), Body).


fabulaNode(Node) :-
    query(T, owlr:classOrSubClassOf, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#FabulaElement'),
	query(Node, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', T, _G).

fabulaNode(RDFList, Node) :-
    member(rdf(Node, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', T, _G), RDFList),
    query(T, owlr:classOrSubClassOf, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#FabulaElement').
   
fabulaCause(RDFList, Node, Cause) :-
    query(P, rdfs:subPropertyOf, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#causes'),
    member(rdf(Cause, P, Node, _G), RDFList).

%================================================================
% ACTUALIZATION of fabula elements
% 
% This section contains predicates that create "real" elements
% from their fabula representation. 
% e.g. (w1 rdf:type Walk) (w1 agens Cinderella) (w1 patiens ...)
%      becomes an Action head that can be scheduled and executed.
%================================================================
        
% Determines whether Node is of a certain class   
nodeClass(RDFList, Class, Node) :-
    member(rdf(Node, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', T, _G), RDFList),
    query(T, owlr:classOrSubClassOf, Class).
        
causalityClass(RDFList, Causer, Caused, Causality) :-
    query(Causality, rdfs:subPropertyOf, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#causes'),
    member(rdf(Causer, Causality, Caused, _G), RDFList).

% Goals        
getFabulaCharacter(RDFList, FabulaNode, FabulaCharacter) :-
	member(rdf(FabulaNode, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#character', FabulaCharacter, _G1), RDFList).
	
getFabulaContents(RDFList, FabulaNode, ContentsRDFList) :-
	member(rdf(FabulaNode, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent', GraphNode, _G1), RDFList),
	findall(rdf(S,P,O,GraphNode), 
		member(rdf(S, P, O, GraphNode), RDFList), 
	ContentsRDFList).
	
% OBSOLETE; semantics changed to GraphNode, rdf:type, fabula:TruthGraph.
getFabulaContentTruth(RDFList, FabulaNode, Truth) :-
	member(rdf(FabulaNode, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent', GraphNode, G1), RDFList),
	member(rdf(GraphNode, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasTruth', Truth, G1), RDFList).


% Creates the head of an Action, based on the following parameters:
%    - RDFList: list of RDF quads representing the fabula describing the Event to be created
%    - ActionNode: the Individual representing the Action in the fabula
%    - (....): the to be returned Action head, checked against its preconditions in the current setting.        
%
% ISSUE with this implementation: if more information is in the fabula than used by the schema, we have the problem
%       that subset(Head, FullHead) fails. A workaround for now is to include agens(Agens), patiens(Patiens), etc in the
%		schema descriptions even when they are not used by the preconditions and the effects, but this is brittle.
createValidatedAction(RDFList, ActionNode, Head) :- 
	createValidatedSchema(RDFList, ActionNode, Head, action).         
	
createValidatedEvent(RDFList, EventNode, Head) :- 
	createValidatedSchema(RDFList, EventNode, Head, event).     
		
createValidatedSchema(RDFList, IndividualNode, Head, Class) :-	
		(
			% IF the agens Agens is a member of the rdf list, 
			member(rdf(IndividualNode, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens', Agens, G1), RDFList) ->
			% THEN make sure the Head unifies only with lists that contain agens(Agens)
			memberchk(agens(Agens), Head) ; true
			
			% Behaviour of this piece of logic shown by:
			%	(member(aap, [aap, mies])-> memberchk(aapIsIn, List) ; true) , (member(kees, [aap, mies]) -> memberchk(keesIsIn, List) ; true).
	     ),
		(
			% Same for patiens
			member(rdf(IndividualNode, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens', Patiens, G1), RDFList) ->
			memberchk(patiens(Patiens), Head) ; true
	     ),
		(
			% Same for target
			member(rdf(IndividualNode, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#target', Target, G1), RDFList) ->
			memberchk(target(Target), Head) ; true
	     ),
		(
			% Same for instrument
			member(rdf(IndividualNode, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#instrument', Instrument, G1), RDFList) ->
			memberchk(instrument(Instrument), Head) ; true
	     ),	     	     
		(
			% Same for type (NOTE: should type not ALWAYS be present?)
			member(rdf(IndividualNode, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Type, G1), RDFList) ->
			memberchk(type(Type), Head) ; true
	     ),	     
		 	
		format('Head: ~p~n', [Head]),
				 	
		% Try all possible schemas...
		schema(S),
		% And select only the heads of events...
		getFromSchema(S, class(Class)),		
		getFromSchema(S, head(FullHead)),
		format('Matching with full head: ~p~n', [FullHead]),
		% To see if the constructed head can be unified with the found heads
		subset(Head, FullHead),
		format('Constructed head is a subset of full head:~nHead: ~p~nFull head: ~p~n.', [Head, FullHead]),
		% And see if it validates.
    	validateOperator(FullHead, [], []) .

    