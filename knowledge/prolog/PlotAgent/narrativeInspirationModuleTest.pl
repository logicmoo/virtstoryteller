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

:- include('paTestEnvironment.pl').
:- include('PlotAgent/suggestions-plopcase.pl').
%:- consult('PlotAgent/narrativeInspirationModule.pl').

:- rdfAssert(('http://dress1','http://www.owl-ontologies.com/FabulaKnowledge.owl#agens','http://cinderella')).
:- rdfAssert(('http://cinderella','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#Human')).
:- rdfAssert(('http://dress1','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Dress')).

:- rdfAssert(('http://lose','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#LoseItem')).
:- rdfAssert(('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#agens','http://cinderella')).
:- rdfAssert(('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens','http://cinderella2')).
:- rdfAssert(('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#target','http://cinderella3')).

:- rdfAssert(('http://cinderella','http://www.owl-ontologies.com/StoryWorldCore.owl#wears','http://cinderella2')).
:- rdfAssert(('http://cinderella2','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#Clothing')).
:- rdfAssert(('http://cinderella3','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#GeographicArea')).

:- rdfAssert(('http://marryking','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal')).
:- rdfAssert(('http://marryking','http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent','http://subgraph')).
:- rdfAssert(('http://cinderella','http://www.owl-ontologies.com/StoryWorldCore.owl#wears','http://dress'),'http://subgraph').
:- rdfAssert(('http://cinderella','http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy','http://palace'),'http://subgraph').



%:- rdfAssert(('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#instrument','http://cinderella4')).

% for getFabulaNodes:
% getFabulaNodes([rdf('http://lose','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#LoseItem', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#agens','http://cinderella', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens','http://cinderella2', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#target','http://cinderella3', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#instrument','http://cinderella4',_)] , Nodes).
% getFabulaCausalities([rdf('http://lose','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#LoseItem', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#agens','http://cinderella', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens','http://cinderella2', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#target','http://cinderella3', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#instrument','http://cinderella4',_), rdf('http://walk','http://www.owl-ontologies.com/FabulaKnowledge.owl#phi_causes','http://lose', _), rdf('http://slordig','http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates','http://lose', _)] , 'http://lose', Causalities).
% eventNode([rdf('http://lose','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#LoseItem', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#agens','http://cinderella', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens','http://cinderella2', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#target','http://cinderella3', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#instrument','http://cinderella4',_)] , Node).
% createEvent([rdf('http://lose','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#LoseItem', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#agens','http://cinderella', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens','http://cinderella2', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#target','http://cinderella3', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#instrument','http://cinderella4',_)], 'http://www.owl-ontologies.com/FabulaKnowledge.owl#LoseItem', E).
% createValidatedEvent([rdf('http://lose','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#LoseItem', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#agens','http://cinderella', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens','http://cinderella2', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#target','http://cinderella3', _)], 'http://lose', E).
% causalLinks([rdf('http://lose','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#LoseItem', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#agens','http://cinderella', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens','http://cinderella2', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#target','http://cinderella3', _),rdf('http://lose','http://www.owl-ontologies.com/FabulaKnowledge.owl#instrument','http://cinderella4',_), rdf('http://walk','http://www.owl-ontologies.com/FabulaKnowledge.owl#phi_causes','http://lose', _), rdf('http://slordig','http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates','http://lose', _)] , 'http://lose', Causalities).
% getFabulaContents([rdf('http://marryking','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal',user), rdf('http://marryking','http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent','http://subgraph', user), rdf('http://cinderella','http://www.owl-ontologies.com/StoryWorldCore.owl#wears','http://dress','http://subgraph'), rdf('http://cinderella','http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy','http://palace','http://subgraph')],'http://marryking',ContentsRDFList).
% getFabulaContents('.'(rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#beInPalace', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal', _27), '.'(rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#beInPalace', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent', 'http://www.owl-ontologies.com/Graphs.owl#graph_2', _28), '.'(rdf('http://www.owl-ontologies.com/StoryWorldCore.owl#cinderella1', 'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy', 'http://www.owl-ontologies.com/StoryWorldCore.owl#palace1', 'http://www.owl-ontologies.com/Graphs.owl#graph_2'), '.'(rdf('http://www.owl-ontologies.com/StoryWorldCore.owl#cinderella1', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.owl-ontologies.com/StoryWorldCore.owl#Human', 'http://www.owl-ontologies.com/Graphs.owl#graph_2'), '.'(rdf('http://www.owl-ontologies.com/StoryWorldCore.owl#palace1', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.owl-ontologies.com/StoryWorldCore.owl#GeographicArea', 'http://www.owl-ontologies.com/Graphs.owl#graph_2'), '.'(rdf(i_attainGoal_1_cinderella, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#beInPalace', _29), [])))))),'http://www.owl-ontologies.com/FabulaKnowledge.owl#beInPalace',FabulaContents).


% validateOperator([type('http://www.owl-ontologies.com/FabulaKnowledge.owl#LoseItem'), agens('http://cinderella'), patiens('http://cinderella2'), target('http://cinderella3')], A, B)
% member(posPreconditions(_G6698), [class(event), head([type('http://www.owl-ontologies.com/FabulaKnowledge.owl#LoseItem'), agens('http://cinderella'), patiens('http://cinderella2'), target(...)]), credibility(1), posPreconditions([ ('http://cinderella', ..., ...), (..., ...)|...]), posEffects([ (..., ...)]), negEffects([...])])


%[rdf('http://marryking','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal',_),
% rdf('http://marryking','http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent','http://subgraph', _),
% rdf('http://cinderella','http://www.owl-ontologies.com/StoryWorldCore.owl#wears','http://dress','http://subgraph'),
% rdf('http://cinderella','http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy','http://palace','http://subgraph')]

%'http://www.owl-ontologies.com/FabulaKnowledge.owl#phi_causes'