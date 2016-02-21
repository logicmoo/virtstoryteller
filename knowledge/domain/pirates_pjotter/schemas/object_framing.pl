% Creating a rum bottle
framing_schema([    
	type(fabula:'CreateRumBottle'),
	preconditions([
		condition(true, [
	        fact(Crate, rdf:type, swc:'TreasureChest')
	    ]),
	    condition(false, [
	    	%fact(ps:oRumBottle_1, swc:limited, true),
	    	fact(ps:oRumBottle_1, swc:containedBy, Crate)	 
	    ])


    ]),
    effects([
		condition(true, [
	        fact(ps:oRumBottle_1, swc:containedBy, Crate)
	    ])    
	    % Test of threats for framing operators.
    ])
]).

framing_schema([
	type(fabula:'CreateCannon'),
	
	preconditions([
		condition(false, [
			fact(Cannon, swc:supportedBy, _),
			fact(Cannon, swc:heldBy, _)
		]),
		condition(true, [
			rule(Cannon, owlr:typeOrSubType, ps:'Cannon'),
			fact(Ship,swc:hasRegion,Loc),
			rule(Ship, owlr:typeOrSubType, ps:'Ship')
		])
	]),
	
	effects([
		condition(true, [
			fact(Cannon, swc:supportedBy, Loc)
		])
	])
]).

    
% Creating a crate for bottles    
framing_schema([    
	type(fabula:'CreateBottleCrate'),
	
    preconditions([
    	condition(false, [  		
	    	fact(ps:oCrate_1, rdf:type, swc:'TreasureChest')
	    ])
    ]),
    effects([
    	condition(true, [    		
		    fact(ps:oCrate_1, rdf:type, swc:'TreasureChest'),
	        fact(ps:oCrate_1, swc:supportedBy, ps:oBeach_1)
	    ])
    ])
]).       
