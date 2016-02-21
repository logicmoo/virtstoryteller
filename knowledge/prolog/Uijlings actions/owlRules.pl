:- use_module(semweb(rdf_db)).


rule(S, owlr:classOrSubClassOf, S):-
	query(S, rdf:type, owl:'Class').

rule(S, owlr:classOrSubClassOf, O):-
	query(S, rdfs:subClassOf, O).

rule(S, owlr:typeOrSubType, O):-
	query(S, rdf:type, Class),
	rule(Class, owlr:classOrSubClassOf, O).
