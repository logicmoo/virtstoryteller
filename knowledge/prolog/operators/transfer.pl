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

% TakeFrom
% PutOn
% Dress
% Undress

operator((
	('http://www.owl-ontologies.com/FabulaKnowledge.owl#TakeFrom', [Agens, Patiens, none, Instrument, CurrLoc]),
	action,
	2,
[
(Instrument,			'http://www.owl-ontologies.com/StoryWorldCore.owl#controlledBy',		Agens),
(Instrument,			swcr:knowsAction,														'http://www.owl-ontologies.com/FabulaKnowledge.owl#TakeFrom'),
(Patiens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			CurrLoc),
(Instrument,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			CurrLoc)
%,(Instrument,			owlr:isNot,																Patiens)
],
[
%(Instrument,			owlr:is,																Patiens)
],
[
(Patiens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#heldBy',				Instrument)
],
[
(Patiens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			CurrLoc)
])
) .

operator((
	('http://www.owl-ontologies.com/FabulaKnowledge.owl#PutOn', [Agens, Patiens, none, Instrument, CurrLoc]),
	action,
	3,
[
(Instrument,			'http://www.owl-ontologies.com/StoryWorldCore.owl#controlledBy',		Agens),
(Instrument,			swcr:knowsAction,														'http://www.owl-ontologies.com/FabulaKnowledge.owl#PutOn'),
(Patiens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#heldBy',				Instrument),
(Instrument,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			CurrLoc)
],
[
],
[
(Patiens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			CurrLoc)
],
[
(Patiens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#heldBy',				Instrument)
])
).

operator((
	('http://www.owl-ontologies.com/FabulaKnowledge.owl#PutOnSelf', [Agens, Patiens, none, Instrument, CurrLoc]),
	action,
	3,
[
(Instrument,			'http://www.owl-ontologies.com/StoryWorldCore.owl#controlledBy',		Agens),
(Instrument,			swcr:knowsAction,														'http://www.owl-ontologies.com/FabulaKnowledge.owl#Dress'),
(Patiens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#heldBy',				Instrument),
(Instrument,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			CurrLoc)
],
[
],
[
(Patiens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			Patiens)
],
[
(Patiens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#heldBy',				Instrument)
])
).

operator((
	('http://www.owl-ontologies.com/FabulaKnowledge.owl#Dress', [Agens, Patiens, none, Instrument]),
	action,
	15,
[
(Instrument,			'http://www.owl-ontologies.com/StoryWorldCore.owl#controlledBy',		Agens),
(Instrument,			swcr:knowsAction,														'http://www.owl-ontologies.com/FabulaKnowledge.owl#Dress'),
(Patiens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#heldBy',				Instrument),
(Patiens, 		owlr:typeOrSubType, 			'http://www.owl-ontologies.com/StoryWorldCore.owl#WearableItem')
%(Patiens, 		rdf:type, 			'http://www.owl-ontologies.com/StoryWorldCore.owl#Clothing')
],
[
],
[
(Patiens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy',				Instrument)
],
[
(Patiens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#heldBy',				Instrument)
])
) .

/*
operator((
	('http://www.owl-ontologies.com/FabulaKnowledge.owl#SimpleDress', [Agens, Patiens, none, Instrument]),
	action,
	15,
[
(Instrument,			'http://www.owl-ontologies.com/StoryWorldCore.owl#controlledBy',		Agens),
(Patiens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#heldBy',				Instrument),
%(Patiens, 		owlr:typeOrSubType, 			'http://www.owl-ontologies.com/StoryWorldCore.owl#WearableItem'),
(Patiens, 		rdf:type, 			'http://www.owl-ontologies.com/StoryWorldCore.owl#Clothing')
],
[
],
[
(Patiens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy',				Instrument)
],
[
(Patiens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#heldBy',				Instrument)
])
) .
*/

operator((
	('http://www.owl-ontologies.com/FabulaKnowledge.owl#Undress', [Agens, Patiens, none, Instrument]),
	action,
	17,
[
(Instrument,			'http://www.owl-ontologies.com/StoryWorldCore.owl#controlledBy',		Agens),
(Instrument,			swcr:knowsAction,														'http://www.owl-ontologies.com/FabulaKnowledge.owl#Undress'),
(Patiens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy',				Instrument)
],
[
],
[
(Patiens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#heldBy',				Instrument)
],
[
(Patiens,		'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy',				Instrument)
])
) .
