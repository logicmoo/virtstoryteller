/* Copyright (C) 2008 Human Media Interaction - University of Twente
 * 
 * This file is part of The Virtual Storyteller.
 * 
 * The Virtual Storyteller is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * The Virtual Storyteller is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with The Virtual Storyteller. If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package vs.plotagent;

import jade.core.AID;

import java.util.Collection;
import java.util.Set;

import vs.IAgentModule;
import vs.communication.RDFtriple;

/**
 * A character manager has the following functionality:
 *    * Keep track of the characters in the agent environment, and their commitments to the story
 *    * Cast characters to give them a place in the story
 *    
 * @author swartjes
 */
public interface ICharacterManager extends IAgentModule {
	

	/**
	 * Adds a character agent to the list of available characters
	 * 
	 * @param characterAgent the AID of an available character to add
	 */
	public void addCharacterAgent(AID characterAgent);
	
	/**
	 * Adds multiple setting changes to be broadcast to all new characters
	 * 
	 * @param triples the setting triples
	 */
	public void addSettingChange (Collection<RDFtriple> triples);	
	
	/**
	 * Adds a setting change to be broadcast to all new characters
	 * 
	 * @param triple the setting triple
	 */
	public void addSettingChange (RDFtriple triple);
	
	/**
	 * Registers that given character URI is necessary to cast. castCharacter uses
	 * this information to decide what name to give a character. 
	 * 
	 * @param characterURI the URI of the character in the story world that we need a character agent for
	 */
	public void addWantedCharacter(String characterURI);
	
	/**
	 * Casts a character agent to play a certain role in the story (role is determined by character manager)
	 * 
	 * @param characterAgent the AID of an available character to add
	 * @param characterURI the URI representing the character Individual in the story
	 */
	public void castCharacter(AID characterAgent, String characterURI);
		
	/**
	 * The inverse: retrieves the agent ID for a character's personality in the story, i.e. which agent 'controls' it.
	 * 
	 * @param characterURI the URI of the casted agent in the story
	 * @return the AID of the character agent playing that character URI in the story
	 */
	public AID getAgentForStoryWorldRepresentation(String characterURI);

	/**
	 * Retrieves all characters that are available to play a role
	 * 
	 * @return a list of character AIDs 
	 */
	public Set<AID> getAvailableCharacters();
	
	/**
	 * Retrieves all characters that are casted to play a role in the story
	 * 
	 * @return a list of character AIDs
	 */	
	public Set<AID> getCastedCharacters();	
	
	/**
	 * Retrieves all characters the character manager knows of
	 * 
	 * @return a set of character AIDs
	 */
	public Set<AID> getCharacters();

	/**
	 * Retrieves the name of the character's personality in the story (i.e., how they are 'casted')
	 * 
	 * @param characterAgent the AID of the casted agent
	 * @return the URI of the character in the story world representation
	 */
	public String getStoryWorldRepresentationForAgent(AID characterAgent);
	
	/**
	 * Removes a character agent from the list of available characters
	 * 
	 * @param characterAgent the AID of an available character to remove
	 */
	public void removeCharacterAgent(AID characterAgent);	
}
