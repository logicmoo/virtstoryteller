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

% action database
% @author kruizingaEE
% @date 2 may 2007

% operator((Head,
%   event,
%	PosPreconditions, NegPreconditions,
%	PosEffects,	NegEffects)) :-
% head(Head), preconditions(PosPreconditions), preconditions(NegPreconditions), effects(PosEffects, effects(NegEffects)
%
% head((actionName, [AgentID, Agens, Patiens, Target, Instrument | Vars])) :- .

% -- LoseClothing --
% One can lose clothing when one wears it, then it is supported by the target location.
operator((
	('http://www.owl-ontologies.com/FabulaKnowledge.owl#LoseItem', [Agens, Patiens, Target, none]),
	event,
	1,
[
	(Agens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#wears',			Patiens),
	(Patiens, 	rdf:type, 				'http://www.owl-ontologies.com/StoryWorldCore.owl#Clothing'),
	(Target,	rdf:type, 				'http://www.owl-ontologies.com/StoryWorldCore.owl#GeographicArea'),
	(Agens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy', 	Target)
],
[],
[
	(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',		Target)
],
[
	(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy',		Agens)
])
).

