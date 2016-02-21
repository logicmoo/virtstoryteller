% ---------------------------------------------------------------------------------
% Copyright (C) 2008 Human Media Interaction - University of Twente
%  
% This file is part of The Virtual Storyteller.
% 
% The Virtual Storyteller is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
% 
% The Virtual Storyteller is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with The Virtual Storyteller. If not, see <http://www.gnu.org/licenses/>.
% ---------------------------------------------------------------------------------

inference_schema([
	type(fabula:'StartExposition'),
	
	preconditions([
		condition(false, [
			fact(fabula:'story_phase', is, _)
		]),
		condition(true, [
			agent(plot)
		])
	]),
	
	effects([
		condition(true, [
			fact(fabula:'story_phase', is, fabula:'exposition')
		])
	])
]).

inference_schema([
	type(fabula:'StartRisingAction'),
	
	preconditions([
		condition(true, [
			fact(fabula:'story_phase', is, fabula:'exposition'),
			agent(plot)
		])
	]),
	
	effects([
		condition(true, [
			fact(fabula:'story_phase', is, fabula:'rising_action')
		]),
		condition(false, [
			fact(fabula:'story_phase', is, fabula:'exposition')
		])
	])
]).

inference_schema([
	type(fabula:'StartFallingAction'),
	
	preconditions([
		condition(true, [
			fact(fabula:'story_phase', is, fabula:'rising_action'),
			agent(plot)
		])
	]),
	
	effects([
		condition(true, [
			fact(fabula:'story_phase', is, fabula:'falling_action')
		]),
		condition(false, [
			fact(fabula:'story_phase', is, fabula:'rising_action')
		])
	])
]).

inference_schema([
	type(fabula:'StartDenouement'),
	
	preconditions([
		condition(true, [
			fact(fabula:'story_phase', is, fabula:'falling_action'),
			agent(plot)
		])
	]),
	
	effects([
		condition(true, [
			fact(fabula:'story_phase', is, fabula:'denouement')
		]),
		condition(false, [
			fact(fabula:'story_phase', is, fabula:'falling_action')
		])
	])
]).
