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
package vs.worldagent;

//import vs.action.IActionManager;
import jade.core.AID;
import vs.communication.PerformOperator;
import vs.rationalagent.IRationalAgent;

/**
 * Provides the access functions for a World Agent
 * @author swartjes
 * Created on 21-jul-2005
 */
public interface IWorldAgent extends IRationalAgent {

	/** Retrieves the Action Database**/
	//public IActionDB getActionDB();

	/** Retrieves the Action Manager**/
//	public IActionManager getActionManager();

	/** Retrieves the Operator Scheduler**/
	public IOperatorScheduler getOperatorScheduler();
	
	/** Retrieves the World Update Manager**/
	//public IWorldUpdateManager getWorldUpdateManager();	
	
	/** Retrieves the Plot Agent that is registered to the World Agent **/
	public AID getPlotAgent();

	/** Handle incoming operators **/
	public boolean handlePerformOperator(PerformOperator po);
	
	/** Get the time of the world (in time steps) **/
	// Moved to IRationalAgent
	//public int getTime();
	
	/** Starts the story, meaning the World Agent becomes 
	 * receptive to actions/events etc.
	 * deprecated since the story now starts upon the first NextRound INFORM of Plot Agent
	 */
	//public void startStory();	
	
	/** Start the next time step
	 * @param roundNumber an int representing the round number (given by Plot Agent's NextRound object)
	 **/
	public void pulse(int roundNumber);
	
	/** Sets the Plot Agent registering to the World Agent **/
	public void setPlotAgent(AID plotAgent);
		
}
