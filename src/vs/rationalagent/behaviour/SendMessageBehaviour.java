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
package vs.rationalagent.behaviour;

import jade.core.Agent;
import jade.core.behaviours.OneShotBehaviour;
import jade.lang.acl.ACLMessage;

import java.util.logging.Logger;

import vs.debug.LogFactory;

/**
 * A simple behaviour to send a message to another agent
 **/

public class SendMessageBehaviour extends OneShotBehaviour
{
	/**
	 * 
	 */
	/////////////////////////////////////////////
	// ATTRIBUTES
	/////////////////////////////////////////////
	private Logger logger;
	protected ACLMessage message;
	
	/////////////////////////////////////////////
	// CONSTRUCTORS AND INSTANTIATION
	/////////////////////////////////////////////
		
	public SendMessageBehaviour(Agent a, ACLMessage msg)	{
		super(a);
		message = msg;

		logger = LogFactory.getLogger(this);
		logger.finer("Creating " +  this.getBehaviourName());
	}

	/////////////////////////////////////////////
	// METHODS
	/////////////////////////////////////////////
	
	@Override
	public void action() {

		logger.fine("Handling " + this.getBehaviourName());
			
		myAgent.send( message );
	}
	
}