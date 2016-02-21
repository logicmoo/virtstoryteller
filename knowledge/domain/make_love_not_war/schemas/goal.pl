% goal database
% @author swartjes
% @date 27 september 2007

% -----------------------------------------------------------


% ------------------------------------------
% TakeOutOnSomebody
% ------------------------------------------
goal_schema([
	type(love:'TakeOutOnSomebody'),
	arguments([agens(Agens), patiens(Patiens) ]),
	preconditions([
		% I am hateful
	    condition(true, [
	        fact(Agens, rdf:type, fabula:'Character'),
	        fact(Agens, love:hasEmotion, love:hate)
	    ]),
	    % And not considerate for others
	    condition(false, [
	    	fact(Agens, love:hasMoralStandard, love:considerateForOthers)
	    ]),
	    % There is someone else
	    condition(true, [
	    	fact(Patiens, rdf:type, fabula:'Character'),
	        rule(Patiens, owlr:isNot, Agens)
	    ])     
	]), 
    success_conditions([
    	% I don't feel hate anymore
        condition(false, [
            fact(Agens, love:hasEmotion, love:hate)
        ])
    ])
]).

	
% ------------------------------------------
% VentButSparePeople
% ------------------------------------------
goal_schema([
	type(love:'VentButSparePeople'),
	arguments([agens(Agens), patiens(Patiens)]),
	preconditions([
	    % I am hateful
	    condition(true, [
	        fact(Agens, rdf:type, fabula:'Character'),
	        fact(Agens, love:hasEmotion, love:hate)
	    ]),
	    % But considerate for others
	    condition(true, [
	    	fact(Agens, love:hasMoralStandard, love:considerateForOthers)
	    ]),	    
	    % There is someone else
	    condition(true, [
	    	fact(Patiens, rdf:type, fabula:'Character'),
	        rule(Patiens, owlr:isNot, Agens)
	    ]),
	    % Who is not hateful
		condition(false, [
			fact(Patiens, love:hasEmotion, love:hate)
		])
	    
	]), 
    success_conditions([
        % I don't feel hate anymore
        condition(false, [
	        fact(Agens, love:hasEmotion, love:hate)
        ]),
	    % But haven't expressed it to someone (EVER - this is wrong)
	    % The planner cannot really deal with this either, will try to cause that either there is no ExpressHate action at all, 
	    % or that Agens hasn't done any actions. That just happens to be always correct in this case, since the agent cannot plan 
	    % actions for others, but we should not rely on this coincidence.
	    condition(false, [
	    	fabula(Act, rdf:type, love:'ExpressHate'),
	    	fabula(Act, fabula:agens, Agens)
	    ])      
    ])
]).

% ------------------------------------------
% SpreadTheWord
% ------------------------------------------
goal_schema([
	type(love:'ShareLove'),
	arguments([agens(Agens), patiens(Patiens)]),
	preconditions([
	    % I am loving
	    condition(true, [
	        fact(Agens, rdf:type, fabula:'Character'),
	        fact(Agens, love:hasEmotion, love:love)
	    ]),
	    % There is someone else
	    condition(true, [
	        fact(Patiens, rdf:type, fabula:'Character'),
	        rule(Patiens, owlr:isNot, Agens)	        
	    ])	    
	    
	]), 
    success_conditions([
        % I have expressed love
        condition(true, [
	        fabula(Act, rdf:type, love:'ExpressLove'),
	        fabula(Act, fabula:agens, Agens)
        ])
    ])
]).

    	
% DEBUG HELPER FUNCTIONS
validGoal(S, P) :-
    goal_schema(S), validate_schema(S, P).
    
validGoalFor(Agens, S, P) :-
    goal_schema(S), schema_agens(S, Agens), validate_schema(S, P).    
    
%validGoalFor(Agens,S,[]),goal_success_conditions(S,C),plan(Agens,C,Plan).    

% plan('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#linda', [condition(true, [fact('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#linda', 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#has', 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#vanilla_ice_1')]), condition(true, [fact('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#linda', 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#at', 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#park_1')])], Plan).