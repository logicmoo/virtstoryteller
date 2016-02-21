% Belief schemas define how facts/perceptions/beliefs can result in new beliefs
%	e.g. seeing someone lying on the floor might lead to the belief that he/she is dead 

schema([
	head([
		type('http://www.owl-ontologies.com/StoryWorldCore.owl#AttendBall'),
		agens(Agens),loc(CurLoc)
		]),
	class(belief),	
	posPreconditions([(Agens, swc:supportedBy, CurLoc),(CurLoc, owlr:isNot, 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#palace')]), 
%	posSuccessConditions([(Agens,  'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#square')])  % go to the square
	posSuccessConditions([(Agens,  swc:supportedBy, 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#palace')])  % go to the square
    ]).    
	
	
