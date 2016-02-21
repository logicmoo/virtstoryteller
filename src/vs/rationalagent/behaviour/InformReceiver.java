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

import jade.content.ContentElement;
import jade.content.lang.Codec.CodecException;
import jade.content.onto.OntologyException;
import jade.core.Agent;
import jade.core.behaviours.CyclicBehaviour;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;

import java.util.logging.Logger;

import vs.IAgent;
import vs.debug.LogFactory;

public abstract class InformReceiver extends CyclicBehaviour {
	
	protected MessageTemplate template;
	protected Logger logger;
	
	public InformReceiver(Agent a) {
		super(a);
		logger = LogFactory.getLogger(this);
		
		// Receive only INFORM messages that match what this agent can understand.
		
		template = MessageTemplate.and( 	
			//MessageTemplate.MatchProtocol(""),
			//MessageTemplate.and(
					MessageTemplate.MatchPerformative(ACLMessage.INFORM), 				
					MessageTemplate.and( 
							MessageTemplate.MatchLanguage( ((IAgent)a).getCodec().getName()),
							MessageTemplate.MatchOntology( ((IAgent)a).getOntology().getName()))
				//	)
			);
		
	}

	/**
	 * Handle the reception of INFORM messages. If an INFORM message is not handled, it is
	 * put back on the message queue for other behaviours.
	 */
	@Override
	public void action() {
		ACLMessage msg = myAgent.receive(template);
		
		if (msg != null) {
			// Handle the message!
			logger.fine("INFORM message received.");

			try {
			
				ContentElement ce = null;
				ce = myAgent.getContentManager().extractContent( msg );
				if (! handleContent(ce, msg) ) {
					// Not handled, put back message
					myAgent.putBack(msg);
				}

			} catch ( CodecException ce ) {
				ce.printStackTrace();
			}
			catch ( OntologyException oe ) {
				oe.printStackTrace();
			}			
	
		} else {
			block();
		}		
	}
	
	public abstract boolean handleContent(ContentElement ce, ACLMessage msg);

}
