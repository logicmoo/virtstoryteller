%%%
% New version by Edze Kruizinga
% @date 26 jan 2007
% added ( ) around action to make the action name available as a variable value

% This is the general action file, which includes preconditions that must hold for each action.
% An action is of the form:
%
% action
% 		(action, (AgentID, Agens, Patiens, Target, Instrument, VarsToBind))
% superAction
% 		(superAction, (AgentID, Agens, Patiens, Target, Instrument, VarsToBind))
% primitiveAction
% 		boolean: is this a primitive action?
% interDuration
% 		integer or formula denoting interDuration
% duration
% 		integer or formula denoting duration
% preconditions
% 		preconditionA and
% 		preconditionB
% 	nextBlock  	% used to denote indepedant precondition blocks.
% 		preconditionC
% interEffects
% 	add
% 		interEffectA and
% 		interEffectB
% 	del
% 		interEffectC
% effects
% 	add
% 		effectA
% 	del
% 		effectB and
% 		effectC
% . % end of action		
%

% Include the rest of the database files.

:- include('actiondb/transitMove.pl').
:- include('actiondb/transfer.pl').

% The superclass 'action':
action
		('http://www.owl-ontologies.com/FabulaKnowledge.owl#Action', (AgentID, Agens, _Patiens, _Target, _Instrument, [])) % changed from "action(" to "(action, (" by Kruizinga to make action a variable
superAction
		none
primitiveAction
		false
preconditions
		% One may not be controlled.
		% This precondition does not allow a sword-fighting horse-rider
		unp (_Controllee, swc:controlledBy, Agens)
	nextBlock
		% One must be controlled by the handling agent.
		(Agens, swc:controlledBy, AgentID)
. % End of action
