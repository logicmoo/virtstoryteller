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
package vs.characteragent;

import jade.core.AID;
import vs.IExplainable;
import vs.communication.CharacterInfo;
import vs.communication.IncomingSetting;
import vs.communication.NextRound;
import vs.communication.OperatorResult;
import vs.communication.StoryAction;
import vs.communication.StoryPerception;
import vs.rationalagent.IRationalAgent;

/**
 * Provides the access functions for the Character Agent
 * @author Kruizinga
 * Created on 1-nov-2006
 */
public interface ICharacterAgent extends IRationalAgent, IExplainable {

	/**
	 * @param args
	 */

	/** Get the AgentID this character agent has in the World **/
	public String getCharacterURI();

	public CharacterProcess getCharacterProcess();
	
	public EpisodicMemory getEpisodicMemory();
	
	public IInterpretationModule getInterpretationModule();
	
	/** Retrieves the Plot Agent that is registered to the Rational Agent **/
	public AID getPlotAgent();
	
	/** Handle CharacterInfo's by setting AgentID to the name given **/
	public void handleCharacterInfo(CharacterInfo c);
	
	/** Handle worldchanges
	 **/
	public void handleIncomingSetting(IncomingSetting is);

	/** Handle nextRound
	 **/
	public void handleNextRound(NextRound n);
	
	/** Handle operator results
	 **/
	public void handleOperatorResult(OperatorResult or);	
	
/*	*//** send fabula element
	 **//*
	public void sendFabulaElement(FabulaElement fe);

	*//** send fabula causality
	 **//*
	public void sendFabulaCausality(FabulaCausality fc);*/

	/** Handle perceptions
	 **/
	public void handlePerception(StoryPerception p);
	
	/** get the action the agent want to perform **/
	public StoryAction handleSelectAction();
	
	/** Set the AgentID this character agent has in the World **/
	public boolean setCharacterURI(String AgentID);
	
	/** Sets the Plot Agent registering to the Rational Agent **/
	public void setPlotAgent(AID plotAgent);
	

}
