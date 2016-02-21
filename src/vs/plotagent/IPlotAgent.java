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

//import vs.action.IActionDB;
import jade.core.AID;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import vs.IExplainable;
import vs.communication.StoryAction;
import vs.fabula.IFabulaBuilder;
import vs.rationalagent.IRationalAgent;

public interface IPlotAgent extends IRationalAgent, IExplainable {

	/** Retrieves the Action Database**/
	//public IActionDB actionDB();
	
	/** Retrieves the Character Manager **/
	public ICharacterManager getCharacterManager();
	
	/** Retrieves the Fabula Builder **/
	public IFabulaBuilder getFabulaBuilder();	
	
	/** Retrieves the Inspiration module **/
	public IInspirationModule getInspirationModule();
	
	/** Retrieves the Perception Manager **/
	public IPerceptionManager getPerceptionManager();
	
	/** Retrieves the Plot Goal Manager **/
	public IPlotGoalManager getPlotGoalManager ();	
	
	/** Retrieves the Episode Manager **/
	public IThreadManager getThreadManager();
	
	/** Getter for (registered) world agent **/
	public AID getWorldAgent();
	
	/**
	 * Deals with incoming action from character agent
	 * @param act the action
	 */
	public void handleIncomingAction(StoryAction act);

	
	
	/**
	 * Deals with incoming subscription info from world and character agents.
	 * This is information about their registering or de-registering.
	 * 
	 * @param dfds the agent description of the agent whose presence the plot agent is subscribed to
	 */
	public void handleSubscription(DFAgentDescription dfds);
	
	/**
	 * Deals with character agents that agree to join the story
	 * 
	 * @param agentID the AID of the agent that joins the story
	 */
	//public void handleJoinedCharacterAgent(AID agentID);
	
	/** Starts the next time step **/
	public void nextRound();
	
	
	/** Setter for world agent
	 * @param worldAgent the AID of the World Agent
	 * **/
	public void setWorldAgent(AID worldAgent);
	
	/** 
	 * Gets the world time (in time steps)
	 * @return the time
	 * MOVED TO IRATIONALAGENT
	 */
//	public int getTime();
	
	// setting manager
	
	// director
	
	// plot goal manager
	
	// event planner

}
