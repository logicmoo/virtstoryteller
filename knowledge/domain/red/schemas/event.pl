event_schema([
	type(red:'BecomeHungry'),
	arguments([agens(Ag)]),
    preconditions([
		% Not hungry
        condition(false, [
        	fact(Ag, swc:hasAttribute, red:hungry)
        ]),
        % was a character 
	    condition(true, [
        	rule(Ag, owlr:typeOrSubType, swc:'Character')
        ])
    ]),
   
    effects([
        % Characters can happen to be hungry
        condition(true, [
        	fact(Ag, swc:hasAttribute, red:hungry)
        ])
    ])
]).	 
