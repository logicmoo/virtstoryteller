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

import java.util.List;

import vs.IAgentModule;
import vs.communication.Operator;

//import vs.knowledge.IJTPKnowledgeManager;


/**
 * Interface for operator scheduler
 * 
 * @author swartjes
 * Created on 15-jul-2005
 */
public interface IOperatorScheduler extends IAgentModule {
	
	/** 
	 * Aborts a scheduled operator
	 * @param sa the scheduled operator to abort
	 **/
	public void abort(ScheduledOperator sa);
	
	/**
	 * Calculates the duration of given Action in a certain knowledge context 
	 * @param theOperator the operator to work with
	 * @return the duration in timesteps
	 */
	public int calcDuration(Operator theOperator);
	
	/**
	 * Calculates the interruptableDuration of given Operator in a certain knowledge context 
	 * @param theOperator the Operator to work with
	 * @return the interruptableDuration in timesteps
	 */
	public int calcInterruptableDuration(Operator theOperator);
	
	/**
	 * Clears the history of scheduled actions
	 */
	public void clearOperatorResults();	
	
	/**
	 * Returns the history of scheduled actions, including their
	 * state (like aborted or finished)
	 * @return a List of previously scheduled actions
	 */
	public List<ScheduledOperator> getOperatorResults();
	
	/** 
	 * Wraps an action into a Scheduled action with begin, inter and end times.
	 * @param a The action to wrap
	 * @return a ScheduledAction
	 */
	//public ScheduledAction calcSchedule(operator a);
		
	/** 
	 * Applies (inter) effects
	 * @param effects the (inter) effects to apply
	 * @param cause the cause of the effects (being the action itself?) 
	 **/
	//public void applyEffects(Vector<Effect> effects, String cause);
	
	/**
	 * Returns the list of scheduled actions
	 */
	public List<ScheduledOperator> getScheduledOperators();
	
	/** 
	 * Increases the time step and handles everything that has to be done in response to it 
	 **/
	public void pulse();	
	
	
	/** 
	 * Performs an undo of intereffects
	 * @param operator the operator to undo intereffects of 
	 **/
	//public void undoOperatorInterEffects(Operator operator);
	
	/** 
	 * Schedules an operator
	 * @param newOperator the operator to schedule
	 * 
	 * @return true if operator was succesfully scheduled and false otherwise
	 **/
	public boolean schedule(Operator newOperator);
	
	/** 
	 * Removes a scheduled operator
	 * @param sa the scheduled operator to remove
	 **/
	public void unschedule(ScheduledOperator sa);
}
