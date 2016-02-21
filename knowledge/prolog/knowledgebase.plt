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

:- begin_tests(knowledgebase).

% --------------
% TEST OWL RULES
% --------------

test(owlr_is1, [nondet]) :-
    %format('Testing owlr:is'),
    query(rule(a, owlr:is, a)).

test(owlr_is2, [fail, nondet]) :-
    query(rule(a, owlr:is, b)).
    
test(owlr_is3, [nondet]) :-
    query(rule(fabula:'FabulaElement', owlr:is, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#FabulaElement')).
    
test(owlr_is4, [fail, nondet]) :-
    query(rule(fabula:'FabulaElement', owlr:is, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#Action')).
    
test(owlr_is5, [nondet]) :-
    query(rule(_A, owlr:is, _B)).   
    
test(owlr_is6, [nondet]) :-
    query(rule(fabula:'FabulaElement', owlr:is, _B)).   
    
test(owlr_is7, [nondet]) :-
    query(rule(_A, owlr:is, fabula:'FabulaElement')).           

test(owlr_is8, [nondet]) :-
    query(rule(A, owlr:is, A)).
    
test(owlr_isnot1, [nondet]) :-
    %format('Testing owlr:isnot'),
    query(rule(a, owlr:isNot, b)).
    
test(owlr_isnot2, [fail, nondet]) :-
    query(rule(a, owlr:isNot, a)).
    
test(owlr_isnot3, [nondet]) :-
    query(rule(fabula:'FabulaElement', owlr:isNot, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#Action')).
 
test(owlr_isnot4, [fail, nondet]) :-
    query(rule(fabula:'FabulaElement', owlr:isNot, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#FabulaElement')).
    
test(owlr_isnot5, [nondet]) :-
    query(rule(_A, owlr:isNot, _B)).
    
test(owlr_isnot6, [fail, nondet]) :-
    query(rule(A, owlr:isNot, A)).    
    
test(owlr_isnot5, [fail, nondet]) :-
    query(rule(A, owlr:isNot, B)), A = jan, B = jan.
    
test(owlr_isnot6, [nondet]) :-
    query(rule(A, owlr:isNot, B)), A = jan, B = piet.    
    
test(owlr_isnot7, [nondet]) :-
    query(rule(A, owlr:isNot, jan)), A = piet.        
    
test(owlr_subclass_form1, [nondet]) :-
    query(rule(fabula:'FabulaElement', owlr:classOrSubClassOf, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#FabulaElement')).

test(owlr_subclass_reasoning1, [nondet]) :-
    query(rule(fabula:'FabulaElement', owlr:classOrSubClassOf, fabula:'FabulaElement')).
    
test(owlr_subclass_reasoning2, [nondet]) :-    
    query(rule(fabula:'Action', owlr:classOrSubClassOf, fabula:'FabulaElement')).
    
test(owlr_type_form1, [nondet]) :-
    query(rule(fabula:'FabulaElement', owlr:typeOrSubType, owl:'Class')).

test(owlr_type_reasoning1, [
        setup(  rdfAssert((fabula:test_ind, rdf:type, fabula:'FabulaElement'))  ),
        cleanup(  rdfRetract((fabula:test_ind, rdf:type, fabula:'FabulaElement'))  ),
        nondet ]) :-  
    query(rule(fabula:test_ind, owlr:typeOrSubType, fabula:'FabulaElement')).
    
test(owlr_type_reasoning2, [
        setup(  rdfAssert((fabula:test_ind, rdf:type, fabula:'Action'))  ),
        cleanup(  rdfRetract((fabula:test_ind, rdf:type, fabula:'Action'))  ),
        nondet ]) :-
    query(rule(fabula:test_ind, owlr:typeOrSubType, fabula:'FabulaElement')).  
    

% --------------
% TEST SWC RULES
% --------------      

test(each_character_has_actions, [nondet]) :-
    query(rule(S, owlr:typeOrSubType, fabula:'Character')),
    query(rule(S, swcr:knowsAction, _O)).    
    
test(length_of_paths1, [
        setup(  rdfAssert( (fabula:test_ind, swc:length, literal(type('http://www.w3.org/2001/XMLSchema#int', 16))) )  ),
        cleanup(  rdfRetract((fabula:test_ind, swc:length, literal(type('http://www.w3.org/2001/XMLSchema#int', 16))) )  ),
        nondet ]) :-
    query(rule(fabula:test_ind, swcr:length, literal(type('http://www.w3.org/2001/XMLSchema#int', 16)))).
    
% No length defined for an individual yields a length of 1.
test(length_of_paths2, [nondet]) :-
    query(rule(fabula:test_nonexistent, swcr:length, literal(type('http://www.w3.org/2001/XMLSchema#int', 1)))).
    
% -------------------
% TEST KNOWLEDGE BASE
% -------------------

test(query, [
        setup(  rdfAssert((fabula:test_ind, rdf:type, fabula:'FabulaElement'))  ),
        cleanup(  rdfRetract((fabula:test_ind, rdf:type, fabula:'FabulaElement'))  ),  
        nondet ]) :-  
    query(fabula:test_ind, rdf:type, fabula:'FabulaElement').
    



:- end_tests(knowledgebase).