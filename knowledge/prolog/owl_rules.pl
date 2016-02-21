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

:- use_module(semweb(rdf_db)).

%rule(S, owlr:is, S).

rule(S, 'http://www.owl-ontologies.com/OWLRules.owl#is', O) :-
	var(S), var(O), !,
	S = O.

rule(S, 'http://www.owl-ontologies.com/OWLRules.owl#is', O) :-
    rdf_db:rdf_global_id(S, S_exp),
    rdf_db:rdf_global_id(O, O_exp),
    owl:owl_same_as(S_exp, O_exp).
    
% Use with care: if any of the variables is uninstantiated, the predicate fails even though 
% the variables might still be bound in such a way that the predicate will succeed.
% Hence the version that works for an uninstantiated rule: 
% true if the vars are not equal.
rule(S, 'http://www.owl-ontologies.com/OWLRules.owl#isNot', O) :-
	var(S), var(O), !,
	dif(S,O).
	
% Version for if only ONE is a variable (redundant).	
rule(S, 'http://www.owl-ontologies.com/OWLRules.owl#isNot', O) :-
	(var(S) ; var(O)), !,
	dif(S,O).	
	
rule(S, 'http://www.owl-ontologies.com/OWLRules.owl#isNot', O) :-
    \+ rule(S, 'http://www.owl-ontologies.com/OWLRules.owl#is', O).



%rule(S, owlr:is, T) :-
%    S = T, ! ;
	%S \= T, 									% avoid a recursive loop with other rules.
    %rdf_db:rdf_global_id(S, S_exp), 			% try to expand to 
	%rdf_db:rdf_global_id(T, S_exp). 			%    same namespace
    
%rule(S, owlr:isNot, O) :-
%	\+ rule(S, owlr:is, O).

% (swartjes) 
% Dit gaat mis: als de linker S geen ge-expande namespace heeft, en de rechter S wel, matcht deze regel niet.
% voorbeeld van een regel die wel zou moeten kloppen maar hierdoor niet klopt: 
% rule(ps:'Treasure', owlr:classOrSubClassOf, 'http://www.owl-ontologies.com/StoryWorldSettings/Pirates#Treasure')

rule(S, 'http://www.owl-ontologies.com/OWLRules.owl#classOrSubClassOf', O) :-
    %rdf_db:rdf_global_id(S, S_exp),
    %rdf_db:rdf_global_id(O, O_exp),
    owl_subclass_of(S, O).
    
rule(S, 'http://www.owl-ontologies.com/OWLRules.owl#directClassOrSubClassOf', O) :-
    rdf_db:rdf_global_id(S, S_exp),
    rdf_db:rdf_global_id(O, O_exp),
    owl_direct_subclass_of(S_exp, O_exp).   

%rule(S, owlr:classOrSubClassOf, S):-
%	query(S, rdf:type, owl:'Class').
	
% Vandaar deze extra regel:	
%rule(S, owlr:classOrSubClassOf, O) :-
%	S \= O, 									% avoid a recursive loop with other rules.
%    rdf_db:rdf_global_id(S, S_exp), 			% try to expand to 
%	rdf_db:rdf_global_id(O, S_exp), 			%    same namespace
%	rule(S_exp, owlr:classOrSubClassOf, S_exp).

%rule(S, owlr:classOrSubClassOf, O):-
%	query(S, rdfs:subClassOf, O).

% S is directly of type O
rule(S, 'http://www.owl-ontologies.com/OWLRules.owl#typeOrSubType', O) :-
    nonvar(O),
    query(S, rdf:type, O).

% S is of a certain type, which is a subclass of O
rule(S, 'http://www.owl-ontologies.com/OWLRules.owl#typeOrSubType', O) :-
    nonvar(O),
    rule(O_Sub, 'http://www.owl-ontologies.com/OWLRules.owl#classOrSubClassOf', O), 
    rdf_db:rdf_global_id(O, O_exp),
    rdf_db:rdf_global_id(O_Sub, Os_exp),
    \+ owl:owl_same_as(O_exp, Os_exp),
    rule(S, 'http://www.owl-ontologies.com/OWLRules.owl#typeOrSubType', O_Sub).    

rule(S, 'http://www.owl-ontologies.com/OWLRules.owl#typeOrSubType', O) :-
    fail, % BUG in owl_individual_of; in process of discussing bug with Jan Wielemaker. In mean time, disable this predicate.
    rdf_db:rdf_global_id(S, S_exp),
    rdf_db:rdf_global_id(O, O_exp),
    % Careful, owl_individual_of/2 expects its second argument to be defined. Doesn't work to retrieve the types of S.
    nonvar(O_exp),
    owl_individual_of(S_exp, O_exp).   

%rule(S, owlr:typeOrSubType, O):-
%	query(S, rdf:type, O), ! ;
%	query(S, rdf:type, Class),
%	rule(Class, owlr:classOrSubClassOf, O).
