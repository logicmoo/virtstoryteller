% action database
% @author kruizingaEE
% @date 2 may 2007

% schema([
% class(action),
% head([
% 	type(),
%	agens(),
%	patiens(),
%	target(),
%	instrument(),
% etc
% ]),
% duration(),
% posPreconditions(),
% negPreconditions(),
% posEffects(),
% negEffects()
% ]).

:- include('transitMove.pl').
:- include('transfer.pl').
:- include('manipulate.pl').

% none action, kan in nieuwe opzet misschien weggelaten worden? (eekruizinga)
%schema([]).
