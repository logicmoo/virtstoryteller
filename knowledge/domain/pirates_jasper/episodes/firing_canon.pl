episode(
	firing_canon(JeanBaptiste, RedBeard),
	[
		(JeanBaptiste, owlr:typeOrSubType, psj:'Pirate'),	%load the two pirates
		(RedBeard, owlr:typeOrSubType, psj:'Pirate'),
		(RedBeard, owlr:isNot, JeanBaptiste)				%and assert they are not the same pirate
	],
	[]
).

episode_procedure(firing_canon(JeanBaptiste, RedBeard)) :-
	% Characters in episode
	episodes:assert(character(JeanBaptiste)),
	episodes:assert(character(RedBeard)),

    % Goals in episode
    % Jean-Baptiste should fire the canon
	goal_schema(S), 
  	getFromSchema(S,head(H)),
    getFromHead(H,agens(JeanBaptiste)),
   	getFromHead(H,type('http://www.owl-ontologies.com/StoryWorldSettings/Pirates_Jasper.owl#FireCanon')),
   	validateOperator(S,[],[]),
   	episodes:assert(goal(JeanBaptiste, S))
   	,   	
   	% While RedBeard climbs to the crows nest
   	goal_schema(S2), 
   	getFromSchema(S2,head(H2)),
    getFromHead(H2,agens(RedBeard)),
   	getFromHead(H2,type('http://www.owl-ontologies.com/StoryWorldSettings/Pirates_Jasper.owl#GoToCrowsNest')),
   	validateOperator(S2,[],[]),
	episodes:assert(goal(RedBeard, S2))
   	.