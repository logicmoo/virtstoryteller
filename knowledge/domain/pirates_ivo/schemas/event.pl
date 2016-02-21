% A water supply is out of water.
event_schema([
	type(ps:'OutOfWater'),
	arguments([	agens(Supply) ]),
	preconditions([
		% There is a full water supply
		condition(true, [
			rule(Supply, owlr:typeOrSubType, ps:'WaterSupply'),
			fact(Supply, ps:hasFullEmptyProperty, ps:full)
		])
	]),
	effects([
		% It is now empty
		condition(true, [			
			fact(Supply, ps:hasFullEmptyProperty, ps:empty)
		]),
		% And no longer true
		condition(false, [			
			fact(Supply, ps:hasFullEmptyProperty, ps:full)
		])
	])
]).
		