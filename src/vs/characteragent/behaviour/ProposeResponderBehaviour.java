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
package vs.characteragent.behaviour;

import jade.content.Concept;
import jade.content.ContentElement;
import jade.content.lang.Codec.CodecException;
import jade.content.onto.OntologyException;
import jade.content.onto.basic.Action;
import jade.core.Agent;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.proto.ProposeResponder;

import java.util.logging.Logger;

import vs.communication.PerformOperator;
import vs.debug.LogFactory;

/**
 * Behaviour for responding to PROPOSE messages from e.g. the Plot Agent.
 * 
 * @author swartjes
 *
 */
public class ProposeResponderBehaviour extends ProposeResponder {
	
	protected Logger logger;
	
	public ProposeResponderBehaviour(Agent a, MessageTemplate mt) {
		super(a, mt);
		logger = LogFactory.getLogger(this);
	}
	
	@Override
	protected ACLMessage prepareResponse(ACLMessage msg) {
		ACLMessage reply = msg.createReply();
		// Assume the worst
		reply.setPerformative(ACLMessage.REJECT_PROPOSAL);
		
		try {
			
			ContentElement ce = null;
			ce = myAgent.getContentManager().extractContent( msg );
			
			Concept actionContent;
			actionContent = ((Action)ce).getAction();
						
			if (actionContent instanceof PerformOperator) {
				// For now, always agree.
				// TODO: check if FramingOperator, and then see if can consistently accept.
				// notion of consistence: 
				//		- preconditions true (simple)
				//		- plans don't change radically (more complex)
				reply.setPerformative(ACLMessage.ACCEPT_PROPOSAL);
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
