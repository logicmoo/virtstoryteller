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
package vs.worldagent.behaviour;

import jade.content.Concept;
import jade.content.ContentElement;
import jade.content.lang.Codec.CodecException;
import jade.content.onto.OntologyException;
import jade.content.onto.basic.Action;
import jade.core.Agent;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.proto.AchieveREResponder;

import java.util.logging.Logger;

import vs.communication.GiveControl;
import vs.communication.PerformOperator;
import vs.debug.LogFactory;
import vs.worldagent.IWorldAgent;

public class RequestResponderBehaviour extends AchieveREResponder {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -6472195387066178022L;
	protected Logger logger;
	
	public RequestResponderBehaviour (Agent a, MessageTemplate mt) {
		super(a, mt);
		
		logger = LogFactory.getLogger(this);

	}
	
	@Override
	protected ACLMessage prepareResponse (ACLMessage msg) {
		// TODO: more advanced implementation where it checks whether it is a plot agent request 
		// and it is already registered with a Plot agent or not, and whether that Plot agent is 
		// still in the system.
				
		// Assume the worst
		ACLMessage reply = msg.createReply();
		reply.setPerformative(ACLMessage.REFUSE);

		try {
			
			ContentElement ce = null;

			ce = myAgent.getContentManager().extractContent( msg );

			Concept actionContent;
			actionContent = ((Action)ce).getAction();
						
			// A request to give control to the Plot Agent
			if (actionContent instanceof GiveControl) {
				if ( (((IWorldAgent)myAgent).getPlotAgent() == null) ||
						((IWorldAgent)myAgent).getPlotAgent().equals(msg.getSender())) {

					// Commit to this Plot agent
					((IWorldAgent)myAgent).setPlotAgent(msg.getSender());

					logger.info("World Agent says: I agree to be controlled.");
					
					reply.setPerformative(ACLMessage.AGREE);
					
				} else {

					logger.info("World Agent says: I refuse to be controlled.");
					reply.setPerformative(ACLMessage.REFUSE);
					
				}
			}
			
			// A request to perform a certain operator
			if (actionContent instanceof PerformOperator) {
								
				if (((IWorldAgent)myAgent).handlePerformOperator((PerformOperator)actionContent)) {
					reply.setPerformative(ACLMessage.AGREE);
				} else {
					reply.setPerformative(ACLMessage.REFUSE);	
				}
				
			}
			
			return reply;
			

		} catch ( CodecException ce ) {
			ce.printStackTrace();
		}
		catch ( OntologyException oe ) {
			oe.printStackTrace();
		}			

		return reply;
		
	}	
	
	protected ACLMessage prepareResultNotification (ACLMessage msg, ACLMessage msg2) {
		return null;
	}

}

//else if (ce instanceof Action) {
//	
//	// Handle an incoming action
//	((IWorldAgent)myAgent).getActionScheduler().schedule((Action)ce);
//	
//	return true;
//	
//} 
