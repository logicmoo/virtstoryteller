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

% NARRATOR module
% TODO: make domain-dependent



:- module(narrator,
	[
		narrate/2,
		schema_template/3,
		object_template/2,
		relation_template/2
	]).

:- use_module('schema_management.pl').
:- use_module('knowledgebase.pl').
:- use_module(library(semweb/rdf_db)).

:- rdf_meta
	narrate(t),
	schema_template(r, -, t),
	object_template(r, -),
	relation_template(t, -).
	
:- multifile
	schema_template/3,
	relation_template/2,
	object_template/2.
	
:- dynamic
	user:term_expansion/2
,	user:goal_expansion/2
.

% Expansion of schema templates
user:term_expansion((schema_template(Ind, Txt, Schema) :- Body), (schema_template(Ind2, Txt, Schema) :- Body)) :-
	rdf_db:rdf_global_id(Ind, Ind2).	
	
% Expansion of schema templates
user:term_expansion((object_template(Ind, Txt) :- Body), (object_template(Ind2, Txt) :- Body)) :-
	rdf_db:rdf_global_id(Ind, Ind2).		
	
% Expansion of schema templates
user:term_expansion((relation_template((S,P,O), Txt) :- Body), (relation_template((S1,P1,O1), Txt) :- Body)) :-
	rdf_db:rdf_global_id(S, S1),
	rdf_db:rdf_global_id(P, P1),
	rdf_db:rdf_global_id(O, O1).	

% Narrate an action or event
narrate(Schema, Text) :-
    nonvar(Schema),
    (	action_schema(Schema) ; event_schema(Schema) ),
    schema_type(Schema, T),
    nonvar(T), % Schema has a type, forget the other narrate clauses.
    narrate_schema(T, Text1, Schema), ! , 
    flatten(Text1, Text2),
    concat_atom(Text2, ' ', Text).
    
% Narrate a framing operator    
narrate(Schema, Text) :-
    nonvar(Schema),
	framing_schema(Schema),
	schema_type(Schema, T),	
	object_template(T, Text), !.

% Narrate an inference operator    
narrate(Schema, Text) :-
    nonvar(Schema),
	inference_schema(Schema),
	schema_type(Schema, T),	
	object_template(T, Text), !.

% Narrate an expectation schema.
narrate(Schema, Text) :-
	nonvar(Schema),
	expectation_schema(Schema),
	schema_type(Schema, T),
	object_template(T, Text), !.

% No narration found for schema.
narrate(Schema, Text) :-
    nonvar(Schema),
	schema(Schema),
	Text2 = ['no narration for schema'],
	concat_atom(Text2, ' ', Text), !.    	    

% Narrate a relation.    
narrate(Rel, Text) :-
    nonvar(Rel),
	relation_template(Rel, Text1),
	flatten(Text1, Text2),
	concat_atom(Text2, ' ', Text), ! .    
    
% No narration found.
narrate(_S, 'no narration').    

narrate_schema(T, Text, Schema) :-
	schema_template(T, Text, Schema)
, 	!
.

narrate_schema(T, Text, Schema) :-
	schema_template_generalize(T, Text, Schema).
	
schema_template_generalize(C, Text, Schema) :-
	schema(Schema),
	query(rule(C, owlr:directClassOrSubClassOf, C2)),
%	format('~w directsubclass ~w~n', [C, C2]),
	narrate_schema(C2, Text2, Schema),
	object_template(C, CDesc),
	Text = [Text2, ' (via ', CDesc, ')'].	
	

% ---- TEMPLATES - keep these in domain-specific files. ----


schema_template('http://www.owl-ontologies.com/FabulaKnowledge.owl#OpenDoor', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_patiens(Schema,Patiens),
    object_template(Agens, AgensDesc),
    object_template(Patiens, PatiensDesc), 
    Text = [AgensDesc, 'opens', PatiensDesc,'(OpenDoor)'].
    
schema_template('http://www.owl-ontologies.com/FabulaKnowledge.owl#Walk', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_target(Schema,Target),
    schema_instrument(Schema,Instrument),
    object_template(Agens, AgensDesc),
    object_template(Target, TargetDesc), 
	object_template(Instrument, InstrumentDesc), 
    Text = [AgensDesc, 'walks to', TargetDesc, 'via', InstrumentDesc, '(Walk)'].
    
    
schema_template('http://www.owl-ontologies.com/FabulaKnowledge.owl#TakeOut', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_patiens(Schema,Patiens),    
    schema_target(Schema,Target),
    object_template(Agens, AgensDesc),
    object_template(Patiens, PatiensDesc), 
    object_template(Target, TargetDesc), 
    Text = [AgensDesc, 'takes', PatiensDesc, 'out of', TargetDesc, '(TakeOut)'].   

schema_template('http://www.owl-ontologies.com/FabulaKnowledge.owl#TakeFrom', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_patiens(Schema,Patiens),    
    schema_target(Schema,Target),
    object_template(Agens, AgensDesc),
    object_template(Patiens, PatiensDesc), 
    object_template(Target, TargetDesc), 
    Text = [AgensDesc, 'takes', PatiensDesc, 'from ', TargetDesc, '(TakeFrom)'].       
    
schema_template('http://www.owl-ontologies.com/FabulaKnowledge.owl#Take', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_patiens(Schema,Patiens),    
    object_template(Agens, AgensDesc),
    object_template(Patiens, PatiensDesc), 
    Text = [AgensDesc, 'takes', PatiensDesc, '(Take)'].   
    
schema_template('http://www.owl-ontologies.com/FabulaKnowledge.owl#Attack', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_patiens(Schema,Patiens),    
    object_template(Agens, AgensDesc),
    object_template(Patiens, PatiensDesc), 
    Text = [AgensDesc, 'attacks', PatiensDesc, '(Attack)'].       
    
schema_template('http://www.owl-ontologies.com/FabulaKnowledge.owl#Transfer', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_patiens(Schema,Patiens),    
    schema_target(Schema,Target),
    object_template(Agens, AgensDesc),
    object_template(Patiens, PatiensDesc),
    object_template(Target, TargetDesc), 
    Text = [AgensDesc, 'transfers', PatiensDesc, 'to', TargetDesc, '(Transfer)'].    
    
    
schema_template('http://www.owl-ontologies.com/FabulaKnowledge.owl#PutOn', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_patiens(Schema,Patiens),    
    schema_target(Schema,Target),
    object_template(Agens, AgensDesc),
    object_template(Patiens, PatiensDesc),
    object_template(Target, TargetDesc), 
    Text = [AgensDesc, 'places', PatiensDesc, 'on', TargetDesc, '(PutOn)'].              
    
% If we are at FabulaElement (superclass), report that there is no template
schema_template('http://www.owl-ontologies.com/FabulaKnowledge.owl#FabulaElement', Text , Schema) :-
	schema(Schema), 
	schema_type(Schema, Type),
	Text = ['no schema template for',Type,'(FabulaElement)'].

%schema_template(C, Text, _) :-
%    Text = ['No template for schema', C].

object_template(URI, Desc) :-
	nonvar(URI),
    uri_label_template(URI, Desc).
    
object_template(URI, Desc) :-
	nonvar(URI),
    uri_type_label_template(URI, Desc).
    
    
object_template(URI, Desc) :-
	var(URI),
	term_to_atom(URI, URIa),
    Desc = ['something (', URIa, ')'] .
    
object_template(URI, Desc) :-    
    Desc = URI.    

% Relation templates
    
relation_template((S, 'http://www.owl-ontologies.com/StoryWorldCore.owl#heldBy', O), Desc) :-
	object_template(S, Sd),
	object_template(O, Od),
	Desc = [Sd, 'is held by', Od].  
	
relation_template((S, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', O), Desc) :-
	object_template(S, Sd),
	object_template(O, Od),
	Desc = [Sd, 'is a', Od], !.	

relation_template((S,P,O), Desc) :-
	object_template(S, Sd),
	object_template(O, Od),
	rdf_db:rdf_global_id(P, P2),
%	rdf_db:rdf_global_id(Ps, P2),
%	term_to_atom(Ps,Pd),
	term_to_atom(P2, Pd),
	Desc = [Sd, Pd, Od], !.	

% Use the rdfs:label of the Individual    
uri_label_template(URI, Desc) :-    
    query(URI, rdfs:label, literal(type(_T, Desc))), !.

% Use the rdfs:label of the Individual        
uri_label_template(URI, Desc) :-
    query(URI, rdfs:label, literal(Desc)), !.
 
% Use the Individual itself
uri_type_label_template(URI, Desc) :-
	rdf_db:rdf_global_id(URI, URI2),
	rdf_db:rdf_global_id(Desc1, URI2),
	term_to_atom(Desc1, Desc). 
    
%uri_type_label_template(URI, ['a', Desc]) :-
%    query(URI, rdf:type, C),
%    URI \= C,
%    object_template(C, Desc).