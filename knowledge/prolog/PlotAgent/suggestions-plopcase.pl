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

suggestion('rule for generating eatApple', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#eatApple', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat', [Vgoal_1],
[  rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_2),
  rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vmain_1),
  rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat','http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_1'),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,'http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_2'),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vmain_1),
  rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat','http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_2'),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,'http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_1'),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,'http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_1'),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_2),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vmain_1),
  rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates',VeatApple,Vmain_1),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,Vmain_1),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_2),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,'http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_2')
])
:- 
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_2),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,Vmain_1),
rdf(Vapple_1,'http://www.owl-ontologies.com/StoryWorldCore.owl#locatedAt',Vhouse_1,Vgraph_1),
rdf(Vbelief_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#BeliefElement',_Vmain_2),
rdf(Vhunger_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Feeling',_Vmain_3),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',Vgraph_1),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_2),
rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes',Vgoal_1,_Vmain_4),
rdf(Vplop_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#Humanoid',_Vmain_5),
rdf(Vgoal_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal',_Vmain_6),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_7),
rdf(Vhouse_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#GeographicArea',Vgraph_1),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates','http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple',_Vmain_8),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_2),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_2,_Vmain_9),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes',Vgoal_1,_Vmain_10),
rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_11),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',_Vmain_12),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_1,_Vmain_13),
Vgraph_1 \= Vgraph_2,Vgraph_2 \= Vgraph_1.

suggestion('rule for generating eatApple', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#eatApple', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat', [Vgoal_1],
[  rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_2),
  rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vmain_1),
  rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat','http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_3'),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,'http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_4'),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vmain_1),
  rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat','http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_4'),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,'http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_3'),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,'http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_3'),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_2),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vmain_1),
  rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates',VeatApple,Vmain_1),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,Vmain_1),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_2),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,'http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_4')
])
:- 
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_2),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,Vmain_1),
rdf(Vapple_1,'http://www.owl-ontologies.com/StoryWorldCore.owl#locatedAt',Vhouse_1,Vgraph_1),
rdf(Vhunger_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Feeling',_Vmain_2),
rdf(Vbelief_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#BeliefElement',_Vmain_3),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',Vgraph_1),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_2),
rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes',Vgoal_1,_Vmain_4),
rdf(Vplop_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#Humanoid',_Vmain_5),
rdf(Vgoal_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal',_Vmain_6),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates',VeatApple,_Vmain_7),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_8),
rdf(Vhouse_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#GeographicArea',Vgraph_1),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates','http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple',_Vmain_9),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_2),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_2,_Vmain_10),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes',Vgoal_1,_Vmain_11),
rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_12),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',_Vmain_13),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_1,_Vmain_14),
Vgraph_1 \= Vgraph_2,Vgraph_2 \= Vgraph_1.

suggestion('rule for generating belief_2', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#belief_2', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#BeliefElement', [VeatApple],
[  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#belief_2','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#BeliefElement',Vmain_1),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes','http://www.owl-ontologies.com/FabulaKnowledge.owl#belief_2',Vmain_1),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_4),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#belief_2','http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#belief_2','http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_4,Vmain_1),
  rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_4),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_4)
])
:- 
rdf(Vhunger_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Feeling',Vmain_1),
rdf(Vbelief_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#BeliefElement',_Vmain_2),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_3),
rdf(Vplop_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#Humanoid',_Vmain_3),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates','http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple',_Vmain_4),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_2),
rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_5),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',_Vmain_6),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_4),
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_2),
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',_Vmain_7),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_8),
rdf(Vapple_1,'http://www.owl-ontologies.com/StoryWorldCore.owl#locatedAt',Vhouse_1,Vgraph_1),
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_3),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_4),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,_Vmain_9),
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_4),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',Vgraph_1),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_3),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_2),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,_Vmain_10),
rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes',Vgoal_1,_Vmain_11),
rdf(Vgoal_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal',_Vmain_12),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates',VeatApple,_Vmain_13),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_14),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_15),
rdf(Vhouse_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#GeographicArea',Vgraph_1),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_2,_Vmain_16),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes',Vgoal_1,_Vmain_17),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_1,_Vmain_18),
Vgraph_1 \= Vgraph_4,Vgraph_1 \= Vgraph_3,Vgraph_1 \= Vgraph_2,Vgraph_4 \= Vgraph_1,Vgraph_4 \= Vgraph_3,Vgraph_4 \= Vgraph_2,Vgraph_3 \= Vgraph_1,Vgraph_3 \= Vgraph_4,Vgraph_3 \= Vgraph_2,Vgraph_2 \= Vgraph_1,Vgraph_2 \= Vgraph_4,Vgraph_2 \= Vgraph_3.

suggestion('rule for generating outcome_1', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#outcome_1', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#Outcome', [Vbelief_2],
[  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#outcome_1','http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#outcome_1','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Outcome',Vmain_1),
  rdf(Vbelief_2,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes','http://www.owl-ontologies.com/FabulaKnowledge.owl#outcome_1',Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#outcome_1','http://www.owl-ontologies.com/FabulaKnowledge.owl#resolves',Vgoal_1,Vmain_1)
])
:- 
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes',Vbelief_2,Vmain_1),
rdf(Vbelief_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#BeliefElement',_Vmain_2),
rdf(Vhunger_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Feeling',_Vmain_3),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_3),
rdf(Vplop_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#Humanoid',_Vmain_4),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates','http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple',_Vmain_5),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_2),
rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_6),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',_Vmain_7),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_4),
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_2),
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',_Vmain_8),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_9),
rdf(Vapple_1,'http://www.owl-ontologies.com/StoryWorldCore.owl#locatedAt',Vhouse_1,Vgraph_1),
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_3),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_4),
rdf(Vbelief_2,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_10),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,_Vmain_11),
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_4),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',Vgraph_1),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_3),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_2),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,_Vmain_12),
rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes',Vgoal_1,_Vmain_13),
rdf(Vbelief_2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#BeliefElement',_Vmain_14),
rdf(Vgoal_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal',_Vmain_15),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates',VeatApple,_Vmain_16),
rdf(Vhouse_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#GeographicArea',Vgraph_1),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_17),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_18),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_2,_Vmain_19),
rdf(Vbelief_2,'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_4,_Vmain_20),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes',Vgoal_1,_Vmain_21),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_1,_Vmain_22),
Vgraph_1 \= Vgraph_4,Vgraph_1 \= Vgraph_3,Vgraph_1 \= Vgraph_2,Vgraph_4 \= Vgraph_1,Vgraph_4 \= Vgraph_3,Vgraph_4 \= Vgraph_2,Vgraph_3 \= Vgraph_1,Vgraph_3 \= Vgraph_4,Vgraph_3 \= Vgraph_2,Vgraph_2 \= Vgraph_1,Vgraph_2 \= Vgraph_4,Vgraph_2 \= Vgraph_3.

suggestion('rule for generating takeApple', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#TakeFrom', [Vgoal_1],
[  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple','http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple','http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple','http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#TakeFrom',Vmain_1),
  rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates','http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple',Vmain_1)
])
:- 
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_2),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,Vmain_1),
rdf(Vapple_1,'http://www.owl-ontologies.com/StoryWorldCore.owl#locatedAt',Vhouse_1,Vgraph_1),
rdf(Vbelief_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#BeliefElement',_Vmain_2),
rdf(Vhunger_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Feeling',_Vmain_3),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',Vgraph_1),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_2),
rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes',Vgoal_1,_Vmain_4),
rdf(Vplop_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#Humanoid',_Vmain_5),
rdf(Vgoal_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal',_Vmain_6),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_7),
rdf(Vhouse_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#GeographicArea',Vgraph_1),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates','http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple',_Vmain_8),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_2),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_2,_Vmain_9),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes',Vgoal_1,_Vmain_10),
rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_11),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',_Vmain_12),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_1,_Vmain_13),
Vgraph_1 \= Vgraph_2,Vgraph_2 \= Vgraph_1.

suggestion('rule for generating sendInvitations', 'http://www.owl-ontologies.com/StoryWorldSetting/ImpactSetting.owl#sendInvitations', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal', [VorganizeGardenContest],
[  rdf('http://www.owl-ontologies.com/StoryWorldSetting/ImpactSetting.owl#sendInvitations','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal',Vmain_1),
  rdf(VorganizeGardenContest,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates','http://www.owl-ontologies.com/StoryWorldSetting/ImpactSetting.owl#sendInvitations',Vmain_1),
  rdf('http://www.owl-ontologies.com/StoryWorldSetting/ImpactSetting.owl#sendInvitations','http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vcarol,Vmain_1)
])
:- 
rdf(VorganizeGardenContest,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal',Vmain_1),
rdf(VorganizeGardenContest,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vcarol,_Vmain_2),
rdf(Vcarol,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Character',_Vmain_3).

suggestion('rule for generating E_stepOnTwig_1', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#E_stepOnTwig_1', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#AccidentallyStepOn', [VA_walk_1],
[  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#E_stepOnTwig_1','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AccidentallyStepOn',Vmain_1),
  rdf(VA_walk_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#phi_causes','http://www.owl-ontologies.com/FabulaKnowledge.owl#E_stepOnTwig_1',Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#E_stepOnTwig_1','http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens','http://www.owl-ontologies.com/StoryWorldCore.owl#twig_1',Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#E_stepOnTwig_1','http://www.owl-ontologies.com/FabulaKnowledge.owl#agens','http://www.owl-ontologies.com/StoryWorldCore.owl#cinderella',Vmain_1)
])
:- 
rdf(VA_walk_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vcinderella_1,Vmain_1),
rdf(VA_walk_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#WalkFromTo',_Vmain_2),
rdf(Vcinderella_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#Human',_Vmain_3).

suggestion('rule for generating E_loseShoe.1', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#E_loseShoe.1', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#LoseItem', [VA_walkfromstairs_1],
[  rdf(VA_walkfromstairs_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#phi_causes','http://www.owl-ontologies.com/FabulaKnowledge.owl#E_loseShoe.1',Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#E_loseShoe.1','http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vcinderella_1,Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#E_loseShoe.1','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#LoseItem',Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#E_loseShoe.1','http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vshoe_1,Vmain_1)
])
:- 
rdf(VA_walkfromstairs_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#WalkFromTo',Vmain_1),
rdf(VA_walkfromstairs_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vcinderella_1,_Vmain_2),
rdf(Vshoe_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#Clothing',_Vmain_3),
rdf(Vcinderella_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#Human',_Vmain_4).

suggestion('rule for generating wearDress', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#wearDress', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal', [VmarryPrince],
[  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#wearDress','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal',Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#wearDress','http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent','http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_5',Vmain_1),
  rdf('http://www.owl-ontologies.com/StoryWorldCore.owl#dress','http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy','http://www.owl-ontologies.com/StoryWorldCore.owl#cinderella1','http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_5'),
  rdf(VmarryPrince,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates','http://www.owl-ontologies.com/FabulaKnowledge.owl#wearDress',Vmain_1),
  rdf('http://www.owl-ontologies.com/StoryWorldCore.owl#cinderella1','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#Human','http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_5'),
  rdf('http://www.owl-ontologies.com/StoryWorldCore.owl#dress','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#Clothing','http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_5'),
  rdf('http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_5','http://www.owl-ontologies.com/FabulaKnowledge.owl#hasTruth','"false"^^http://www.w3.org/2001/XMLSchema#boolean',Vmain_1)
])
:- 
rdf(VmarryPrince,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal',Vmain_1).

suggestion('rule for generating goal_1', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#goal_1', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal', [Vbelief_1,Vhunger_1],
[  rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes','http://www.owl-ontologies.com/FabulaKnowledge.owl#goal_1',Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#eatApple','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat','http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_6'),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#goal_1','http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#goal_1','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal',Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#goal_1','http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates','http://www.owl-ontologies.com/FabulaKnowledge.owl#eatApple',Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#goal_1','http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates','http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple',Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#eatApple','http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,'http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_6'),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#goal_1','http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent','http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_6',Vmain_1),
  rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes','http://www.owl-ontologies.com/FabulaKnowledge.owl#goal_1',Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#eatApple','http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,'http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_6')
])
:- 
rdf(Vplop_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#Humanoid',Vmain_1),
rdf(Vapple_1,'http://www.owl-ontologies.com/StoryWorldCore.owl#locatedAt',Vhouse_1,Vgraph_1),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_2),
rdf(Vhouse_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#GeographicArea',Vgraph_1),
rdf(Vbelief_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#BeliefElement',_Vmain_3),
rdf(Vhunger_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Feeling',_Vmain_4),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',Vgraph_1),
rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_5),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_1,_Vmain_6),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',_Vmain_7).

suggestion('rule for generating goal_1', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#goal_1', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal', [Vbelief_1,Vhunger_1],
[  rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes','http://www.owl-ontologies.com/FabulaKnowledge.owl#goal_1',Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#eatApple','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat','http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_7'),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#goal_1','http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#goal_1','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal',Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#goal_1','http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates','http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple',Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#eatApple','http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,'http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_7'),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#goal_1','http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent','http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_7',Vmain_1),
  rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes','http://www.owl-ontologies.com/FabulaKnowledge.owl#goal_1',Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#eatApple','http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,'http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_7')
])
:- 
rdf(Vplop_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#Humanoid',Vmain_1),
rdf(Vapple_1,'http://www.owl-ontologies.com/StoryWorldCore.owl#locatedAt',Vhouse_1,Vgraph_1),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_2),
rdf(Vhouse_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#GeographicArea',Vgraph_1),
rdf(Vbelief_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#BeliefElement',_Vmain_3),
rdf(Vhunger_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Feeling',_Vmain_4),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',Vgraph_1),
rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_5),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_1,_Vmain_6),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',_Vmain_7).

suggestion('rule for generating see_1', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#see_1', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#Hear', [VeatApple],
[  rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_3),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#see_1','http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#see_1','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Hear',Vmain_1),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_3),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_3),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#phi_causes','http://www.owl-ontologies.com/FabulaKnowledge.owl#see_1',Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#see_1','http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_3,Vmain_1)
])
:- 
rdf(Vbelief_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#BeliefElement',Vmain_1),
rdf(Vhunger_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Feeling',_Vmain_2),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_3),
rdf(Vplop_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#Humanoid',_Vmain_3),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates','http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple',_Vmain_4),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_2),
rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_5),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',_Vmain_6),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_4),
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_2),
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',_Vmain_7),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_8),
rdf(Vapple_1,'http://www.owl-ontologies.com/StoryWorldCore.owl#locatedAt',Vhouse_1,Vgraph_1),
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_3),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_4),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,_Vmain_9),
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_4),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',Vgraph_1),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_3),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_2),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,_Vmain_10),
rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes',Vgoal_1,_Vmain_11),
rdf(Vgoal_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal',_Vmain_12),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates',VeatApple,_Vmain_13),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_14),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_15),
rdf(Vhouse_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#GeographicArea',Vgraph_1),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_2,_Vmain_16),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes',Vgoal_1,_Vmain_17),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_1,_Vmain_18),
Vgraph_1 \= Vgraph_4,Vgraph_1 \= Vgraph_3,Vgraph_1 \= Vgraph_2,Vgraph_4 \= Vgraph_1,Vgraph_4 \= Vgraph_3,Vgraph_4 \= Vgraph_2,Vgraph_3 \= Vgraph_1,Vgraph_3 \= Vgraph_4,Vgraph_3 \= Vgraph_2,Vgraph_2 \= Vgraph_1,Vgraph_2 \= Vgraph_4,Vgraph_2 \= Vgraph_3.

suggestion('rule for generating beInPalace', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#beInPalace', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal', [VmarryPrince],
[  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#beInPalace','http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent','http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_8',Vmain_1),
  rdf('http://www.owl-ontologies.com/StoryWorldCore.owl#cinderella1','http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy','http://www.owl-ontologies.com/StoryWorldCore.owl#palace1','http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_8'),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#beInPalace','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal',Vmain_1),
  rdf('http://www.owl-ontologies.com/StoryWorldCore.owl#cinderella1','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#Human','http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_8'),
  rdf(VmarryPrince,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates','http://www.owl-ontologies.com/FabulaKnowledge.owl#beInPalace',Vmain_1),
  rdf('http://www.owl-ontologies.com/StoryWorldCore.owl#palace1','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#GeographicArea','http://www.owl-ontologies.com/Graphs.owl##isubgraph_plotagent_8')
])
:- 
rdf(VmarryPrince,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal',Vmain_1).

suggestion('rule for generating see_1', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#see_1', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#See', [VeatApple],
[  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#see_1','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#See',Vmain_1),
  rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_3),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#see_1','http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,Vmain_1),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_3),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_3),
  rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#phi_causes','http://www.owl-ontologies.com/FabulaKnowledge.owl#see_1',Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#see_1','http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_3,Vmain_1)
])
:- 
rdf(Vhunger_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Feeling',Vmain_1),
rdf(Vbelief_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#BeliefElement',_Vmain_2),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_3),
rdf(Vplop_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#Humanoid',_Vmain_3),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates','http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple',_Vmain_4),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_2),
rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_5),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',_Vmain_6),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_4),
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_2),
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',_Vmain_7),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_8),
rdf(Vapple_1,'http://www.owl-ontologies.com/StoryWorldCore.owl#locatedAt',Vhouse_1,Vgraph_1),
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_3),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_4),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,_Vmain_9),
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_4),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',Vgraph_1),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_3),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_2),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,_Vmain_10),
rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes',Vgoal_1,_Vmain_11),
rdf(Vgoal_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal',_Vmain_12),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates',VeatApple,_Vmain_13),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_14),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_15),
rdf(Vhouse_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#GeographicArea',Vgraph_1),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_2,_Vmain_16),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes',Vgoal_1,_Vmain_17),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_1,_Vmain_18),
Vgraph_1 \= Vgraph_4,Vgraph_1 \= Vgraph_3,Vgraph_1 \= Vgraph_2,Vgraph_4 \= Vgraph_1,Vgraph_4 \= Vgraph_3,Vgraph_4 \= Vgraph_2,Vgraph_3 \= Vgraph_1,Vgraph_3 \= Vgraph_4,Vgraph_3 \= Vgraph_2,Vgraph_2 \= Vgraph_1,Vgraph_2 \= Vgraph_4,Vgraph_2 \= Vgraph_3.

suggestion('rule for generating takeApple', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#TakeFrom', [Vgoal_1],
[  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple','http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple','http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple','http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vmain_1),
  rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple','http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#TakeFrom',Vmain_1),
  rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates','http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple',Vmain_1)
])
:- 
rdf(VeatApple,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Eat',Vgraph_2),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,Vmain_1),
rdf(Vapple_1,'http://www.owl-ontologies.com/StoryWorldCore.owl#locatedAt',Vhouse_1,Vgraph_1),
rdf(Vhunger_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#Feeling',_Vmain_2),
rdf(Vbelief_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#BeliefElement',_Vmain_3),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',Vgraph_1),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens',Vapple_1,Vgraph_2),
rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes',Vgoal_1,_Vmain_4),
rdf(Vplop_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#Humanoid',_Vmain_5),
rdf(Vgoal_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/FabulaKnowledge.owl#AttainGoal',_Vmain_6),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates',VeatApple,_Vmain_7),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_8),
rdf(Vhouse_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#GeographicArea',Vgraph_1),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#motivates','http://www.owl-ontologies.com/FabulaKnowledge.owl#takeApple',_Vmain_9),
rdf(VeatApple,'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens',Vplop_1,Vgraph_2),
rdf(Vgoal_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_2,_Vmain_10),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#psi_causes',Vgoal_1,_Vmain_11),
rdf(Vhunger_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#character',Vplop_1,_Vmain_12),
rdf(Vapple_1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.owl-ontologies.com/StoryWorldCore.owl#FruitOrVegetable',_Vmain_13),
rdf(Vbelief_1,'http://www.owl-ontologies.com/FabulaKnowledge.owl#hasContent',Vgraph_1,_Vmain_14),
Vgraph_1 \= Vgraph_2,Vgraph_2 \= Vgraph_1.

