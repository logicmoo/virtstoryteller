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

% WalkFromTo
% WalkToFrom

operator((
	('http://www.owl-ontologies.com/FabulaKnowledge.owl#WalkFromTo', [Agens, none, Target, Instrument, CurrLoc, Road, Length]),
	action,
	1,
[
(Instrument,			'http://www.owl-ontologies.com/StoryWorldCore.owl#controlledBy',		Agens),
(Instrument,			swcr:knowsAction,														'http://www.owl-ontologies.com/FabulaKnowledge.owl#WalkFromTo'),
(Road, 			rdf:type, 	'http://www.owl-ontologies.com/StoryWorldCore.owl#GroundWay'),
(Road,			'http://www.owl-ontologies.com/StoryWorldCore.owl#toGeographicArea',	Target),
(Road,			'http://www.owl-ontologies.com/StoryWorldCore.owl#fromGeographicArea',	CurrLoc),
(Road,			'http://www.owl-ontologies.com/StoryWorldCore.owl#length',				Length),
(Instrument,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			CurrLoc)
],
[
],
[
(Instrument,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',		Target)
],
[
(Instrument,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',		CurrLoc)
])
).

operator((
	('http://www.owl-ontologies.com/FabulaKnowledge.owl#WalkToFrom', [Agens, none, Target, Instrument, CurrLoc, Road, Length]),
	action,
	1,
[
(Instrument,			'http://www.owl-ontologies.com/StoryWorldCore.owl#controlledBy',		Agens),
(Instrument,			swcr:knowsAction,														'http://www.owl-ontologies.com/FabulaKnowledge.owl#WalkToFrom'),
(Road, 	rdf:type, 	'http://www.owl-ontologies.com/StoryWorldCore.owl#GroundWay'),
(Road,	'http://www.owl-ontologies.com/StoryWorldCore.owl#fromGeographicArea',	Target),
(Road,	'http://www.owl-ontologies.com/StoryWorldCore.owl#toGeographicArea',	CurrLoc),
(Road,	'http://www.owl-ontologies.com/StoryWorldCore.owl#length',				Length),
(Instrument,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',			CurrLoc)
],
[
],
[
(Instrument,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',		Target)
],
[
(Instrument,			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',		CurrLoc)
])
).