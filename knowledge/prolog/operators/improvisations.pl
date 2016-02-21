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

% improvisation database
% @author kruizingaEE
% @date 13-06-2007

% operator((improvisation,Head, Duration,
%	PosPreconditions, NegPreconditions,
%	PosEffects,	NegEffects)) :-
% head(Head),
%% duration(Duration),
% preconditions(PosPreconditions), preconditions(NegPreconditions),
% effects(PosEffects), effects(NegEffects)
%
% head((actionName, [Agens, Patiens, Target, Instrument | Vars])) :- ?.

/*
operator((
	('http://www.owl-ontologies.com/FabulaKnowledge.owl#CreateClothing', [_Agens, Patiens, none, Instrument, CurrLoc]),
	improvisation,
	1,
[
],
[
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			_Somewhere)
],
[
(Patiens,			rdf:type,									'http://www.owl-ontologies.com/StoryWorldCore.owl#Clothing'),
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			CurrLoc)
],
[
])
).
*/

operator((
	('http://www.owl-ontologies.com/FabulaKnowledge.owl#CreateHelper', [Agens, Patiens, none, Instrument, CurrLoc]),
	improvisation,
	1,
[
(Instrument,				'http://www.owl-ontologies.com/StoryWorldCore.owl#controlledBy',		Agens),
(Instrument,				'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			CurrLoc),
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#improvroom'),
(Patiens,			swcr:type,																'http://www.owl-ontologies.com/StoryWorldCore.owl#Organism')
],
[
],
[
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#controlledBy',		Agens),
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			CurrLoc)
],
[
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#improvroom')
])
).

operator((
	('http://www.owl-ontologies.com/FabulaKnowledge.owl#GetFromMagicStore', [Agens, Patiens, none, Instrument, CurrLoc]),
	improvisation,
	1,
[
(Instrument,				'http://www.owl-ontologies.com/StoryWorldCore.owl#controlledBy',		Agens),
(Instrument,				'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			CurrLoc),
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#improvroom')
%(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			Target)
%(Patiens,			swcr:type,																'http://www.owl-ontologies.com/StoryWorldCore.owl#Organism')
],
[
],
[
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			CurrLoc)
],
[
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#improvroom')
%(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			Target)
])
).

/*
operator((
	('http://www.owl-ontologies.com/FabulaKnowledge.owl#MagicTakeControl', [_Agens, Patiens, none, Instrument, none]),
	improvisation,
	1,
[
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#improvroom'),
(Patiens,			swcr:type,																'http://www.owl-ontologies.com/StoryWorldCore.owl#Organism')
],
[
],
[
(Patiens,			'http://www.owl-ontologies.com/StoryWorldCore.owl#controlledBy',			Agens)
],
[
])
).
*/