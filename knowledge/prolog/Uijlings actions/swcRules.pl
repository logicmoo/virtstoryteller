% This rulebase is a quick hack for my demo
%
:- use_module(semweb(rdf_db)).

%%%
% Determines if two objects are at the same location.
rule(S, swcr:atLocation, O):-
	query(S, swc:locatedAt, O),
	query(O, swc:isLowestLevel, true).

rule(S, swcr:atLocation, S):-
	query(S, swc:isLowestLevel, true).


rule(S, swcr:subClassOf, O):-
	rdf_db:rdf_global_id(S, Class),
	rdf_db:rdf_global_id(O, Class),
	query(Class, rdf:type, owl:'Class').
rule(S, swcr:subClassOf, O):-
	query(S, rdfs:subClassOf, O).

%%%
% Temporary validTransferLocation rule
% kruizinga 9 feb 2007
rule(S, swcr:validTransferLocation, O):-
	query(S, swcr:atLocation, O).

%%%
% Determines if an object is of a certain class.
rule(S, swcr:type, O):-
	query(S, rdf:type, O).

rule(S, swcr:type, O):-
	query(S, rdf:type, T),
	query(T, rdfs:subClassOf, O).

% Determines the height of an object
rule(S, swcr:totalHeight, 0):-
	query(S, swcr:type, swc:'GeographicArea').

rule(S, swcr:totalHeight, Height):-
	query(S, swc:supportedBy, Loc),
	query(Loc, swc:isLowestLevel, true),
	query(S, swc:height, Height).

rule(S, swcr:totalHeight, Height):-
	query(S, swc:supportedBy, Obj),
	query(Obj, swcr:type, swc:'CorpuscularObject'),
	rule(Obj, swcr:totalHeight, ObjHeight),
	query(S, swc:height, SHeight),
	Height is ObjHeight + SHeight.

rule(S, swcr:totalHeight, Height):-
	query(S, swc:attachedBy, Obj),
	query(Obj, swcr:type, swc:'CorpuscularObject'),
	rule(Obj, swcr:totalHeight, Height).

rule(S, swcr:totalHeight, Height):-
	query(S, swc:containedBy, Obj),
	query(Obj, swcr:type, swc:'CorpuscularObject'),
	rule(Obj, swcr:totalHeight, Height).

rule(S, swcr:totalHeight, Height):-
	query(S, swc:wornBy, Obj),
	query(Obj, swcr:type, swc:'CorpuscularObject'),
	rule(Obj, swcr:totalHeight, Height).

% totalWeight
%
% Determines the total weight of an object plus the objects above it
rule(S, swcr:stapleWeight, Weight):-
	% get the set of objects which are located at S
	setof(X, query(X, swc:locatedAt, S), ObjectsLocatedAtS),
	addWeightsOfObjects(ObjectsLocatedAtS, Weight).



%%%%%%% Helper Functions

% addWeightsOfObjects gets the combined weight of the objects in the set.

% List is empty
addWeightsOfObjects([], 0).

addWeightsOfObjects([Object|RestObjects], TotalWeight):-
	addWeightsOfObjects(RestObjects, RestWeight),
	query(Object, swc:weight, Weight),
	TotalWeight is Weight + RestWeight.
