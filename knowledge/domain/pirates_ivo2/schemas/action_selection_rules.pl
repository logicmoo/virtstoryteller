% _______________________
% Action selection rules.
% _______________________


% --------------------------------------------------------------------
% AnnounceRum
% A character will say "Let's get some rum" when he wants to get some.
% --------------------------------------------------------------------
action_selection_schema([
	type(ps:'AnnounceRumRule'),
	arguments([ agens(Agens) ]),
	preconditions([
		% There is a GetBottle goal that I have (had)
		condition(true, [
			fabula(S, rdf:type, ps:'GetBottle'),
			fabula(S, fabula:character, Agens)
		]),
		% And that hasn't motivated me to announce it yet
		% (for now: and I haven't said "let's get some rum" yet)
		condition(false, [
			fabula(T, rdf:type, ps:'SayLetsGetSomeRum'),
			fabula(T, fabula:character, Agens)
		])		
	]),
	selected_action(Act)
]) 	
	:-
action_schema(Act), 
schema_type(Act, ps:'SayLetsGetSomeRum'),
schema_agens(Act, Agens),
validate_schema(Act, []).
	

% --------------------------------------
% AnnounceFightIntention
% A character will say "Prepare to die".
% --------------------------------------
action_selection_schema([
	type(ps:'AnnounceFightIntentionRule'),
	arguments([ agens(Agens), patiens(Patiens) ]),
	preconditions([
		% There is a WoundEnemy goal that I have (had)
		% TODO: it must be the active goal, no?
		condition(true, [
			fabula(S, rdf:type, ps:'WoundEnemy'),
			fabula(S, fabula:agens, Agens),
			fabula(S, fabula:patiens, Patiens)
		]),
		% And that hasn't motivated me to say "Prepare to die" yet
		% TODO: when motivation is implemented, use fabula:motivates
		% for now, check whether "SayEnGarde" was already executed with given agens and patiens.
		condition(false, [
			fabula(T, rdf:type, ps:'SayEnGarde'),
			fabula(T, fabula:agens, Agens),
			fabula(T, fabula:patiens, Patiens)
		])	
	]),
	selected_action(Act)
]) 	
	:-
action_schema(Act), 
schema_type(Act, ps:'SayEnGarde'),
schema_agens(Act, Agens),
schema_patiens(Act, Patiens),
validate_schema(Act, []).
	
% ---------------------------------------------
% AnnounceDefendIntention
% A character will say "No way I'm gonna die!".
% ---------------------------------------------
action_selection_schema([
%	type(ps:'AnnounceDefendIntentionRule'),
%	arguments([ agens(Agens), patiens(Patiens) ]),
	preconditions([
		% There is a GetBottle goal that I have (had)
		condition(true, [
			fabula(S, rdf:type, ps:'Defend'),
			fabula(S, fabula:agens, Agens),
			fabula(S, fabula:patiens, Patiens)
		]),
		% And that hasn't motivated me to announce it yet
		% TODO: when motivation is implemented, use fabula:motivates
		% for now, check whether "AnnounceFightIntention" has already been a goal.
		% Careful, this currently (28 jul 08) means: "there is no AnnounceFightIntention instance OR I am not the character of anything"
		condition(false, [
			fabula(T, rdf:type, ps:'SayNoWay'),
			fabula(T, fabula:agens, Agens),
			fabula(T, fabula:patiens, Patiens)
		])
	]),
	selected_action(Act)
]) 	
	:-
action_schema(Act), 
schema_type(Act, ps:'SayNoWay'),
schema_agens(Act, Agens),
schema_patiens(Act, Patiens),
validate_schema(Act, []).	
	
	
	
	
		
		