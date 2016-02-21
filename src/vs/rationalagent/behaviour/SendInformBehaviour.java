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
import jade.core.AID;
import jade.core.Agent;
import jade.core.behaviours.OneShotBehaviour;
import jade.lang.acl.ACLMessage;

import java.util.logging.Logger;

import vs.debug.LogFactory;
import vs.rationalagent.RationalAgent;

/**
 * A behaviour to send an INFORM message to another agent
 **/

public class SendInformBehaviour extends OneShotBehaviour
{
	/**
	 * 
	 */
	private static final long serialVersionUID = -5450755780589217811L;
	/////////////////////////////////////////////
	// ATTRIBUTES
	/////////////////////////////////////////////
	private ContentElement m_content;
	private AID[] m_target;
	private Logger logger;
	
	/////////////////////////////////////////////
	// CONSTRUCTORS AND INSTANTIATION
	/////////////////////////////////////////////
		
	public SendInformBehaviour(Agent a, AID[] target, ContentElement ce)	{
		super(a);
		m_content = ce;
		m_target = target;
		logger = LogFactory.getLogger(this);
		logger.finer("Creating " +  this.getBehaviourName() + " for " + ce.getClass());
	}

	/////////////////////////////////////////////
	// METHODS
	/////////////////////////////////////////////
	
	@Override
	public void action() {

		logger.fine("Handling " + this.getBehaviourName() + " for " + m_content);
		
		ACLMessage msg = new ACLMessage( ACLMessage.INFORM );
		
		for (AID element : m_target) {
			logger.fine("Adding receiver: " + element);
			msg.addReceiver( element );
		}

		
		
		msg.setLanguage( ((RationalAgent)myAgent).getCodec().getName());
		msg.setOntology( ((RationalAgent)myAgent).getOntology().getName());

		try {
			myAgent.getContentManager().fillContent( msg, m_content );
			myAgent.send( msg );
			logger.fine("Message sent.");
		}
		catch( CodecException ce ) {
			logger.warning("CodecException! (see stdout)");
			ce.printStackTrace();
		}
		catch( OntologyException oe ) {
			logger.warning("OntologyException! (see stdout)");
			oe.printStackTrace();
		}
	}
	
}