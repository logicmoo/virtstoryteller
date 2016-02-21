
framing_schema([
	type(red:'BeMean'),
	scope(all),
    preconditions([
      	% Nobody else is mean already
    	condition(false, [
    		fact(_, swc:hasAttribute, red:mean)
    	]),
		% Agens is a character
		condition(true, [
			rule(Ag, owlr:typeOrSubType, swc:'Character')
		])

    ]),
   
    effects([
        % Characters can happen to be mean
        condition(true, [
        	fact(Ag, swc:hasAttribute, red:mean)
        ])
    ])
]).	   