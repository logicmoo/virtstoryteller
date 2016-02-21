% schema database
% @author Ivo Swartjes
% @date 29 oktober 2007

% Build up schema database (in order of expected use count): 
% schema definitions for story domain

% There are schema definitions in below files (in order of expected use count).
%:- consult(schemas:'action.pl').
%:- consult(schemas:'goal.pl').
%:- consult(schemas:'improvisation.pl').
%:- consult(schemas:'event.pl').

:- consult('action.pl').
:- consult('goal.pl').
:- consult('improvisation.pl').
:- consult('event.pl').	
:- consult('expectation.pl').	
:- consult('action_selection_rules.pl').

