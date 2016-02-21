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

% This rulebase is a quick hack for my demo
%
:- use_module(semweb(rdf_db)).

rule(S, 'http://www.owl-ontologies.com/SWCRules.owl#knowsAction', O) :-
	query(S, 'http://www.owl-ontologies.com/StoryWorldCore.owl#hasAction', A),
	query(rule(O, 'http://www.owl-ontologies.com/OWLRules.owl#classOrSubClassOf', A)).

%%%
% Determines if two objects are at the same location.
rule(S, 'http://www.owl-ontologies.com/SWCRules.owl#atLocation', O):-
	query(S, 'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy', O),
	query(O, 'http://www.owl-ontologies.com/StoryWorldCore.owl#isLowestLevel', true).

rule(S, 'http://www.owl-ontologies.com/SWCRules.owl#atLocation', S):-
	query(S, 'http://www.owl-ontologies.com/StoryWorldCore.owl#isLowestLevel', true).
	
rule(S, 'http://www.owl-ontologies.com/SWCRules.owl#locatedAtTransitive', T):-
	query(S, 'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy', T).	
	
rule(S, 'http://www.owl-ontologies.com/SWCRules.owl#locatedAtTransitive', T):-
	query(T1, 'http://www.owl-ontologies.com/StoryWorldCore.owl#partOfGeographicArea', T)
,	rule(S, 'http://www.owl-ontologies.com/SWCRules.owl#locatedAtTransitive', T1).		

% OBSOLETE
%rule(S, swcr:subClassOf, O):-
%	rdf_db:rdf_global_id(S, Class),
%	rdf_db:rdf_global_id(O, Class),
%	query(Class, rdf:type, owl:'Class').
	
%rule(S, swcr:subClassOf, O):-
%	query(S, rdfs:subClassOf, O).

%%%
% Temporary validTransferLocation rule
% kruizinga 9 feb 2007
%rule(S, swcr:validTransferLocation, O):-
%	query(S, swcr:atLocation, O).
	
%%% 
% Determines if we can get from A to B
% TODO: doesn't check things like locked doors etc.
% swartjes 23 december 2007
rule(L2, 'http://www.owl-ontologies.com/SWCRules.owl#reachableFrom', L1) :- 
	query(rule(Ro, 	owlr:typeOrSubType, 'http://www.owl-ontologies.com/StoryWorldCore.owl#TransitWay')),
	query(Ro,	'http://www.owl-ontologies.com/StoryWorldCore.owl#fromGeographicArea',	L1),
	query(Ro,	'http://www.owl-ontologies.com/StoryWorldCore.owl#toGeographicArea',	L2).
	
rule(L2, 'http://www.owl-ontologies.com/SWCRules.owl#reachableFrom', L1) :-
	query(Ro, 	owlr:typeOrSubType, 'http://www.owl-ontologies.com/StoryWorldCore.owl#TransitWay'),
	query(Ro,	'http://www.owl-ontologies.com/StoryWorldCore.owl#fromGeographicArea',	L1),
	query(Ro,	'http://www.owl-ontologies.com/StoryWorldCore.owl#toGeographicArea',	Ltemp),
	Ltemp \= L2,
	query(rule(L2, 'http://www.owl-ontologies.com/SWCRules.owl#reachableFrom', Ltemp)).
	

%%%
% OBSOLETE
% Determines if an object is of a certain class.
%rule(S, swcr:type, O):-
%	query(S, rdf:type, O).

% OBSOLETE
%rule(S, swcr:type, O):-
%	query(S, rdf:type, T),
%	query(T, rdfs:subClassOf, O).
	
%%%
% Determines the length of a path; if not defined, returns 1
rule(S, 'http://www.owl-ontologies.com/SWCRules.owl#length', O) :-
    query(S, 'http://www.owl-ontologies.com/StoryWorldCore.owl#length', O), ! ; 
    O = literal(type('http://www.w3.org/2001/XMLSchema#int', 1)).
