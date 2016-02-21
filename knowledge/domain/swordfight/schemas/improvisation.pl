framing_schema([    
	type(ps:'CreateRapier'),
    preconditions([
		% A belt, worn by the agens.       
    	condition(true, [        
			fact(Belt, swc:wornBy, Ag),
    		rule(Belt, owlr:typeOrSubType, ps:'Belt')
    	]),	       	
        
        % There exists a rapier.
		condition(true, [
	        rule(Rapier, owlr:typeOrSubType, ps:'Rapier'),
        	rule(Ag, owlr:typeOrSubType, swc:'Human')
        ])
    ]),
   
    effects([
        % Now wearing the rapier
        condition(true, [
        	fact(Rapier, swc:wornBy, Ag)
        ])
    ])
]).   
    
framing_schema([    
    type(ps:'CreateBelt'),
    preconditions([
        % There exists a belt, and a human
		condition(true, [
		    rule(Belt, owlr:typeOrSubType, ps:'Belt'),
        	rule(Ag, owlr:typeOrSubType, swc:'Human')
        ]),

        % Not already somewhere
		condition(false, [
		    fact(Belt, swc:locatedAt, _)
		])
	]),
    effects([   
        % Now wearing belt
		condition(true, [
	        fact(Belt, swc:wornBy, Ag)	
	    ])
    ])
]).       