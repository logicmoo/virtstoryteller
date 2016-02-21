%:- include('./experiment_1/framing_operators_from_setting.pl').

% -----------------------
% Creating a rum bottle
% -----------------------
framing_schema([    
	type(ps:'RumBottleInContainer'),
	preconditions([
		% There exists a container
		condition(true, [
	        fact(Crate, rdf:type, swc:'Container')
	    ]),
	    % There exists a bottle
	    condition(true, [
	        fact(Bottle, rdf:type, ps:'Bottle')
	    ]),
	    % The bottle is not located anywhere yet
	    condition(false, [
	    	fact(Bottle, swc:locatedAt, _)
	    ])
    ]),
    effects([
		% Pretend the rum bottle is in the container
		condition(true, [
	        fact(Bottle, swc:containedBy, Crate)
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
	type(ps:'BeCaptain'),
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
	    % Careful, this currently (28 jul 08) means "there is no captain, or nobody owns a ship"
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

framing_schema([    
	type(ps:'CreateRapier'),
    preconditions([

        % There exists a rapier and a pirate
		condition(true, [
	        rule(Rapier, owlr:typeOrSubType, ps:'Rapier')
	    ]),
	    condition(true, [
        	rule(Ag, owlr:typeOrSubType, ps:'Pirate')
        ]),
		% Not already somewhere
    	condition(false, [        
			fact(Rapier, swc:wornBy, _)
    	]),	       	
    	condition(false, [        
			fact(Rapier, swc:locatedAt, _)
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
	type(ps:'HateYou'),
    preconditions([

        % There exists two pirates
	    condition(true, [
        	rule(Ag, owlr:typeOrSubType, ps:'Pirate'),
        	rule(Pa, owlr:typeOrSubType, ps:'Pirate'),
        	rule(Ag, owlr:isNot, Pa)        	
        ]),
		% There is no hate in the story yet
        condition(false, [
        	fact(_, ps:hates, _)
        ]) 	    	      
    ]),
   
    effects([
        % Pirates can "just" hate each other
        condition(true, [
        	fact(Ag, ps:hates, Pa)
        ])
    ])
]).   
       