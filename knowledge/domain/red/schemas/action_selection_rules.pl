% _______________________
% Action selection rules.
% _______________________


% --------------------------------------------------------------------
% Greet
% A character will greet.
% --------------------------------------------------------------------
action_selection_schema([
	type(red:'GreetRule'),
	arguments([ agens(Agens), target(Target) ]),
	preconditions([
		% One will greet if one hasn't yet greeted
		condition(false, [
			fact(Agens, red:greeted, Target)
		]),
		condition(false, [
			fact(Target, red:greeted, Agens)
		])
	]),
	selected_action(Act)
]) 	
	:-
action_schema(Act), 
schema_type(Act, red:'Greet'),
schema_agens(Act, Agens),
schema_target(Act, Target),
validate_schema(Act, []).	

% --------------------------------------------------------------------
% GreetBack
% A character will greet back if it has just been greeted.
% --------------------------------------------------------------------
action_selection_schema([
	type(red:'GreetBackRule'),
	arguments([ agens(Agens), target(Target) ]),
	preconditions([
		% Target just greeted me
		condition(true, [
			fact(Target, red:greeted, Agens)
		]),
		% One will greet back if one hasn't yet greeted back
		condition(false, [
			fact(Agens, red:greeted, Target)
		])
	]),
	selected_action(Act)
]) 	
	:-
action_schema(Act), 
schema_type(Act, red:'GreetBack'),
schema_agens(Act, Agens),
schema_target(Act, Target),
validate_schema(Act, []).	

% --------------------------------------------------------------------
% Cry
% A little girl will cry when something she owns has been taken from her.
% TODO: something like: when sadness > X (with decay). Now, Red keeps crying.
% For now, just do it once.
% --------------------------------------------------------------------
action_selection_schema([
	type(red:'CryRule'),
	arguments([ agens(Agens), patiens(Patiens)]),
	preconditions([
		% I am a little girl
		condition(true, [
			rule(Agens, owlr:typeOrSubType, red:'LittleGirl')
		]),
		% That hasn't cried yet
		condition(false, [
			fabula(I, rdf:type, red:'Cry'),
			fabula(I, fabula:agens, Agens),
			fabula(I, fabula:endtime, _)
		]),
		% I own something
		condition(true, [
			fact(Agens, swc:owns, Patiens)
		]),
		% But I don't have it now
		condition(false, [
			fact(Agens, swc:has, Patiens)
		]),
		% Because someone has taken it from me.
		condition(true, [
			fabula(Ind, rdf:type, red:'TakeFrom'),
			fabula(Ind, fabula:target, Agens),
			fabula(Ind, fabula:patiens, Patiens),
			fabula(Ind, fabula:endtime, _)	% Must have been successful!
		])
	]),
	selected_action(Act)
]) 	
	:-
action_schema(Act), 
schema_type(Act, red:'Cry'),
schema_agens(Act, Agens),
validate_schema(Act, []).	

% --------------------------------------------------------------------
% CommunicatePoison
% When adopting a "Poison" goal, one will tell.
% --------------------------------------------------------------------
action_selection_schema([
	type(red:'TellAboutPoisonPlanRule'),
	arguments([ agens(Agens)]),
	preconditions([
		% I'll do this whenever possible, but only once. 
		condition(false, [
			fabula(I, rdf:type, red:'TellAboutPoisonPlan'),
			fabula(I, fabula:agens, Agens)
		])
	]),
	selected_action(Act)
]) 	
	:-
action_schema(Act), 
schema_type(Act, red:'TellAboutPoisonPlan'),
schema_agens(Act, Agens),
validate_schema(Act, []).	
	
		
		