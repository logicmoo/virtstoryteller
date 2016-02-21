% Expectation schemas

/*
expectation_schema([
	type(red:'ExpectWillBeTakenAndEaten'),
	arguments([agens(Agens), patiens(Patiens), target(Target)]),
    preconditions([

        % A character
	    condition(true, [
        	rule(Target, owlr:typeOrSubType, swc:'Character')
        ]),
        % Which is hungry
        condition(true, [
        	fact(Target, swc:hasAttribute, red:hungry)
        ]),
        % And mean
        condition(true, [
        	fact(Target, swc:hasAttribute, red:mean)
        ]),
        % And at same location as another character (e.g., me)
        condition(true, [
        	fact(Agens, swc:at, Loc),
        	fact(Target, swc:at, Loc)
        ]),
        % And the other character has food
		condition(true, [
   	        rule(Patiens, owlr:typeOrSubType, red:'Food')
   	    ]),
   	    condition(true, [
   	    	fact(Agens, swc:has, Patiens)
   	    ]),
   	    % Which is poisoned
   	    condition(true, [
   	    	fact(Patiens, swc:hasAttribute, red:poisoned)
   	    ])
   	    
    ]),
   
    effects([
        % You can expect them to get it, eat it and die
        condition(true, [
        	fact(Target, swc:hasAttribute, red:dead)
        ])
    ])
]).	  
*/

% Expecting AGENS to eat PATIENS
expectation_schema([
	type(red:'ExpectEat'),
	arguments([agens(Agens), patiens(Patiens)]),
    preconditions([

        % A character
	    condition(true, [
        	rule(Agens, owlr:typeOrSubType, swc:'Character')
        ]),
        % Which is hungry
        condition(true, [
        	fact(Agens, swc:hasAttribute, red:hungry)
        ]),
        % And has food
		condition(true, [
   	        rule(Patiens, owlr:typeOrSubType, red:'Food')
   	    ]),
   	    condition(true, [
   	    	fact(Agens, swc:has, Patiens)
   	    ])
   	    
    ]),
   
    effects([
        % Will eat it
        % Problem: Act might bind with action already in the fabula, in which case these statements form
        % 			appendices to this action.
        %		   So if the wolf has already eaten a birthday cake, the effect of this expectation is that 
        %		    that action has as agens also Agens and as patiens also Patiens
        /*condition(true, [
        	fabula(Act, rdf:type, red:'Eat'),
        	fabula(Act, fabula:agens, Agens),
        	fabula(Act, fabula:patiens, Patiens)
        ])*/
    ])
]).	

% Expecting AGENS to die from TARGET
expectation_schema([
	type(red:'ExpectDieFrom'),
	arguments([agens(Agens), target(Target)]),
    preconditions([
	    % TARGET was poisoned
	    condition(true, [
	    	fact(Target, swc:hasAttribute, red:poisoned)
        ]),  
        % but AGENS has eaten it
        % Problem: Act might bind with action already in the fabula, in which case the effects of ExpectEat form
        % 			appendices to this action.
        %		   So if the wolf has already eaten a birthday cake, the effect of ExpectEat is that 
        %		    that action has as agens also Agens and as patiens also Patiens        
	    condition(true, [
	    	fabula(Act, rdf:type, red:'ExpectEat'),
	    	fabula(Act, fabula:agens, Agens),
	    	fabula(Act, fabula:patiens, Target)
	    ])
 	    
    ]),
   
    effects([
        % AGENS will die
        condition(true, [
        	fact(Agens, swc:hasAttribute, red:dead)
        ])
    ])
]).	 

% Expecting someone to die from something
/*
expectation_schema([
	type(red:'ExpectTakeFrom'),
	arguments([agens(Agens), patiens(Patiens), target(Target)]),
    preconditions([

        % A character and another
	    condition(true, [
			rule(Agens, owlr:typeOrSubType, fabula:'Character'),
			rule(Target, owlr:typeOrSubType, fabula:'Character')
		]),
		% Character has food
		condition(true, [
			rule(Patiens, owlr:typeOrSubType, red:'Food')
		]),
		condition(true, [
			fact(Agens, swc:has, Patiens)
		]),
	    % Character is mean 
	    condition(true, [
	    	fact(Agens, swc:hasAttribute, red:mean)
        ]), 
        % Character is hungry
        condition(true, [
        	fact(Agens, swc:hasAttribute, red:hungry)
        ]),
        % Characters are on same location
        condition(true, [
        	fact(Agens, swc:at, Loc),
        	fact(Target, swc:at, Loc)
        ])
       
    ]),
   
    effects([
        % They will get it eventually
        condition(true, [
        	fabula(Target, swc:has, Patiens)
        ])
    ])
]).	     */
