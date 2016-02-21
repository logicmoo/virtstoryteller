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

% operator database
% @author kruizingaEE
% @date 28 september 2007

% operator((Head,
%	action,
%	PosPreconditions, NegPreconditions,
%	PosEffects,	NegEffects)) :-
% head(Head), preconditions(PosPreconditions), preconditions(NegPreconditions), effects(PosEffects, effects(NegEffects)
%
% head((actionName, [Agens, Patiens, Target, Instrument | Vars])) :- .

:- include('actions.pl').
:- include('improvisations.pl').
:- include('events.pl').
