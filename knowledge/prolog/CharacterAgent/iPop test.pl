/*

% Goals I use for testing:
('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cloak', 		'http://www.owl-ontologies.com/StoryWorldCore.owl#heldBy',	'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella')
('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#chickensuit',	'http://www.owl-ontologies.com/StoryWorldCore.owl#heldBy',	'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella')
('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella',	'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',		'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#palace')
('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cloak', 		'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy',	'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella')
pop((7, 30000, 0), [('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cloak','http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy','http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#house')], [], P), showPlan(P).

pop((7, 30000, 0), [('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#slipper1','http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy','http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#house')], [], P), showPlan(P).

tryPop([('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cloak', 'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#square')]).

tryPop([('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella',		'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',		'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#palace')]).

manyPlan(5, [('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella',		'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',		'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#palace'), ('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#chickensuit', 'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella')], N1, N2).

manyGood(100, 1.1, [('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella',		'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',		'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#palace'), ('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cloak', 'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella')], N1, N2).

manyPlan(3, [('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cloak', 'http://www.owl-ontologies.com/StoryWorldCore.owl#heldBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella')], N1, N2).

many(6, [('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#nicedress3', 'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella'), ('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella',	'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',		'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#palace')], N1, N2).

pop((3, 30000, 2), [('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#nicedress3', 'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella')], [], P), showPlan(P).

pop((3, 30000, 1), [('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#gown', 'http://www.owl-ontologies.com/StoryWorldCore.owl#heldBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella')], [], P), showPlan(P).

pop((4, 30000, 2), [('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#gown', 'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#house')], [], P), showPlan(P).
pop((3, 30000, 2), [('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#godmother', 'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#house'), ('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#gown', 'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#house')], [], P), showPlan(P).
pop((4, 30000, 1), [('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#godmother', 'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#house'), ('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#nicedress3', 'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella')], [], P), showPlan(P).
pop((3, 30000, 2), [('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#godmother', 'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#house')], [], P), showPlan(P).

pop((5, 30000, 1), [(SomeClothes, 'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella'), ('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella',		'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',		SomePlace), ('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#prince',		'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',		SomePlace)], [], P), showPlan(P).

pop((5, 30000, 1), [('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#chickensuit', 'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella'), ('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella',		'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',		'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#square')], [], P), showPlan(P).

storeMultiPlan(80, 1.1, [('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella','http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy','http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#palace'), ('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cloak', 'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella')], []).

manyPlan(3, [('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cloak', 'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella')], N1, N2).

tryPopC([
	('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cloak',		'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy',		'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella')
	,('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella',		'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',		'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#palace')
		]).

tryPop([	
 	('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#prince1',			'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',	PrinceLocation)
 	,('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella',	'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',	PrinceLocation)
 	,(_Something,																	'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy',		'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella')
 	%,('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cloak',		'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy',		'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella')
	]).

tryPop([	
 	('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella',	'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy',	'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#workum1')
	]).

pop(19,[('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cloak','http://www.owl-ontologies.com/StoryWorldCore.owl#heldBy','http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella')],[],P), P = (Steps, Orderings, Links, Counters), member((_X, _Y, ('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella','http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy','http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#palace'), none), Links, Counters), showPlan(P).

idPop(13, [('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella', 'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#palace'), ('http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cloak', 'http://www.owl-ontologies.com/StoryWorldCore.owl#wornBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella')], [], P).

pop((7, 15, 0), [(ps:oRumBottle_1, swc:heldBy, ps:leChuck)], [], P).
pop((7, 15, 1), [('http://www.owl-ontologies.com/StoryWorldSettings/Pirates#leChuck', 'http://www.owl-ontologies.com/StoryWorldCore.owl#supportedBy', 'http://www.owl-ontologies.com/StoryWorldSettings/Pirates#oHold_1')], [], P).

pop((5, 5, 1), [(ps:oRumBottle_1, swc:heldBy, ps:leChuck)], [], P).
pop:iterative_pop(depth, (8,20,2), [(ps:oRumBottle_1, swc:heldBy, ps:leChuck)], [], Plan).
pop:iterative_pop(depth, (8,20,2), [(ps:oRumBottle_1, swc:heldBy, ps:leChuck)], [], Plan), !, planStep(Plan, (Name, Op)), narrate(Op, Txt).

planner:plan([(ps:oRumBottle_1, swc:heldBy, ps:leChuck)], [], Plan).
planner:plan([(ps:oRumBottle_1, swc:heldBy, ps:leChuck), (ps:leChuck, swc:supportedBy, ps:oDeck_1)], [], Plan).
plan([(lolli:linda, lolli:has, lolli:vanilla_ice_1)], [], Plan).

*/
