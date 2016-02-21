event_schema([
	type(ps:'ResupplyRum'),
	preconditions([
	    condition(false, [
			fact(ps:oRumBottle_1, swc:limited, true)
	    ]),	
		condition(true, [
	        fact(Crate, rdf:type, swc:'Container')
	    ])
	]),
	effects([
		condition(true, [
	        fact(ps:oRumBottle_1, swc:containedBy, Crate)
	    ])	
	])
]).

