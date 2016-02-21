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

import vs.IAgentModule;
import vs.communication.Operator;

/**
 * Interface for World Knowledge Manager
 * 
 * @author swartjes
 * Created on 20-sep-2005
 */
public interface IWorldUpdateManager extends IAgentModule {
	
	/**
	 * Requests a world update, world knowledge manager executes this update if the requested changes do
	 * not lead to inconsistenties
	 * @param updates world state to apply, mapped to a cause (ie an Action) or null if no cause.
	 * @return whether the world update was executed
	 */
	//public boolean requestWorldUpdates(Vector<RDFtriple> updates, String cause);
	
	/**
	 * Apply effects to the world without a consistency check
	 * Store the knowledge as updated knowledge
	 * @param effects the effects to apply
	 */
	//public void performWorldUpdates(Vector<RDFtriple> effects);
		
	/**
	 * Ask knowledgebase to apply operator effects to the world without a consistency check
	 * @param operator to apply
	 */
	public void performOperator(Operator operator);
	
	/** 
	 * Retrieve recent knowledge updates, for Plot agent
	 * @return new knowledge updates
	 */	
	//public Map<Vector<RDFtriple>,String> getWorldUpdates();
	
	/**
	 * Clear all knowledge updates
	 *
	 */
	//public void clearWorldUpdates();

}
