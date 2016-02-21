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

import java.util.Vector;

import vs.IAgentModule;

/**
 * The task of the thread manager is to set up story environments and characters 
 * so that it matches the description of the beginning of a thread.
 * 
 * The initial design is that threads have preconditions, setting definitions and character goals.
 * The thread manager should contain functionality to:
 * 		- see if an thread is applicable
 * 		- determine whether thread was already used
 * 		- remember which thread is currently being used
 * 		- setup the environment for the thread
 * 
 *  A question is how much of this should be implemented in Prolog, and how much in Java.
 * 
 * @author swartjes
 *
 */
public interface IThreadManager extends IAgentModule {
	
	public enum StoryPhase {none, exposition, rising_action, falling_action, denouement};
	
	/**
	 * Tries to start the given thread. If it is not already possible, it will
	 * be added to the plot goal manager in an attempt to make this possible.
	 */
	public void addThread(String threadString);
	
	/**
	 * Executes the thread, meaning that setting and characters are set up. 
	 * 
	 * @param thread the thread to execute.
	 */
	public void executeThread(PlotThread thread);

	/**
	 * Determines what the current thread is
	 * 
	 * @return a prolog string representing the thread identifier
	 */
	public PlotThread getCurrentThread();
	
	
	/**
	 * Signals that a thread is finished (i.e. that a character has achieved his resolution goal
	 */
	public void finishThread (PlotThread thread);
	
	/**
	 * Determines all possible threads and returns their Prolog identifier strings
	 * 
	 * @return the prolog strings representing the thread identifiers (e.g. thread-on-ship(X))
	 */
	public Vector<String> getPossibleThreads();
	
	/**
	 * Retrieves the episodic goals, defined for certain character, if any.
	 * 
	 * @param characterURI the URI of the character to retrieve episodic goals for
	 * 
	 * @return its episodic goals
	 */
	public Vector<Goal> getThreadGoals(String characterURI);
	
	/**
	 * Returns whether the thread manager is trying to start an thread at this point
	 * @return true if an thread is being started, false if not
	 */
	public boolean startingThread ();

	/**
	 *  Assigns a resolution goal to a character.
	 *  
	 *  @param goal the goal to assign to the character (the character is embedded in the goal)
	 *  @param thread the plot thread this assignment comes from; used for logging purposes
	 */
	public void assignResolutionGoal (Goal goal, PlotThread thread);

	
	/**
	 * Tells thread manager to do something (e.g. start new thread)
	 */
	//public void pulse();
}
