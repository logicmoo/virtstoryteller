% action database for love-not-war
% @author swartjes
% @date 21 jan 2008


action_schema([
	type(love:'ExpressHate'),
    arguments([agens(Agens), target(Target)	]),
	duration(1),
	preconditions([
		% I am a hateful character
        condition(true, [
            fact(Agens, rdf:type, fabula:'Character'),
	        fact(Agens, love:hasEmotion, love:hate)
		]),
		% There is someone else I can express my hate to
        condition(true, [
	        fact(Target, rdf:type, fabula:'Character'),
	        rule(Agens, owlr:isNot, Target)        
		])				
	]),
	effects([
		% I am not a hateful character anymore
		condition(false, [
		    fact(Agens, love:hasEmotion, love:hate)
		]),
		% Someone else is hateful now
		condition(true, [
		    fact(Target, love:hasEmotion, love:hate)
		])
    ])
]).

action_schema([
	type(love:'ExpressLove'),
    arguments([agens(Agens), target(Target)	]),
	duration(1),
	preconditions([
		% I am a  loving character
        condition(true, [
	        fact(Agens, rdf:type, fabula:'Character'),
	        fact(Agens, love:hasEmotion, love:love)        
		]),
		% There is someone else I can express my love to
        condition(true, [
	        fact(Target, rdf:type, fabula:'Character'),
	        rule(Agens, owlr:isNot, Target)        
		])		
	]),
	effects([
        % Love only multiplies :-)
    ])
]).

action_schema([
	type(love:'KickTree'),
    arguments([agens(Agens)	]),
	duration(1),
	preconditions([
		% I am a hateful character
        condition(true, [
	        fact(Agens, rdf:type, fabula:'Character'),
	        fact(Agens, love:hasEmotion, love:hate)        
		])
	]),
	effects([
		% I am no longer a hateful character
		condition(false, [
		    fact(Agens, love:hasEmotion, love:hate)
		])
    ])
]).