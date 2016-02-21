% -----------------------
% Creating a rum bottle
% -----------------------
framing_schema([    
	type(ps:'CreateRumBottle'),
	preconditions([
		condition(true, [
			% If there is a container
	        fact(Crate, rdf:type, swc:'Container')
	    ]) 
    ]),
    effects([
		condition(true, [
			% Then the rum bottle could be in it
	        fact(ps:oRumBottle_1, swc:containedBy, Crate)
	    ])    
    ])
]).   
    
% ----------------------------    
% Creating a crate for bottles
% ----------------------------    
framing_schema([    
	type(ps:'BottleCrateInHold'),
	
    preconditions([
		% If there is a container
    	condition(true, [  		
	    	fact(Crate, rdf:type, swc:'Container')
	    ]),
		% And it is not already somewhere
	    condition(false, [
	    	fact(Crate, swc:supportedBy, _)
	    ])
    ]),
    effects([
	    % Then it could be in the hold
    	condition(true, [    		
	        fact(Crate, swc:supportedBy, ps:oHold_1)
	    ])
    ])
]).       

% ------------------------------------
% Creating a water supply for the ship
% ------------------------------------
framing_schema([    
	type(ps:'ShipHasWaterSupply'),
	preconditions([
		% There is a ship
		condition(true, [
	        rule(Ship, owlr:typeOrSubType, ps:'Ship')
	    ]),
	    % There is a water supply
		condition(true, [
	        rule(Supply, owlr:typeOrSubType, ps:'WaterSupply')
	    ]),	    
	    % Which is not already on some ship
	    condition(false, [
	    	fact(_, ps:hasWaterSupply, Supply)
	    ])
    ]),
    effects([
		condition(true, [
			% Our ship has the water supply
	        fact(Ship, ps:hasWaterSupply, Supply)
	    ])    
    ])
]).   

% ------------------------------------
% Becoming the captain of a ship
% ------------------------------------
framing_schema([    
	type(ps:'BecomeCaptain'),
	preconditions([
		% There is a ship
		condition(true, [
	        rule(Ship, owlr:typeOrSubType, ps:'Ship')
	    ]),
		% There is a character
		condition(true, [
	        rule(Character, owlr:typeOrSubType, fabula:'Character')
	    ]),	    
	    % The ship has no captain yet
	    % Careful, this currently (28 jul 08) means: "There is no captain, or nobody owns a ship"
		condition(false, [
	        rule(Captain, owlr:typeOrSubType, ps:'Captain'),
	        fact(Captain, ps:owns, Ship)	        
	    ]),
	    % Character is not already captain (of another ship)
	    condition(false, [
	    	rule(Character, owlr:typeOrSubType, ps:'Captain')
	    ])
    ]),
    effects([
		condition(true, [
			% The ship has the character as captain
	        fact(Character, rdf:type, ps:'Captain'),
	        fact(Character, ps:owns, Ship)
	    ])
    ])
]).   