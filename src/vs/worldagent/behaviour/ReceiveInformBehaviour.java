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

import jade.content.ContentElement;
import jade.core.Agent;
import jade.lang.acl.ACLMessage;

import java.util.Iterator;
import java.util.logging.Logger;

import vs.communication.IncomingSetting;
import vs.communication.NextRound;
import vs.communication.RDFtriple;
import vs.communication.StorySettingElement;
import vs.debug.LogFactory;
import vs.knowledge.PrologKB;
import vs.rationalagent.behaviour.InformReceiver;
import vs.worldagent.IWorldAgent;

/**
 * Behaviour to receive INFORM messages for the World Agent
 * 
 * @author swartjes
 *
 */
public class ReceiveInformBehaviour extends InformReceiver {

	/**
	 * 
	 */
	private static final long serialVersionUID = 4287807288946992460L;
	private Logger logger;
	
	public ReceiveInformBehaviour(Agent a) {
		super(a);
		logger = LogFactory.getLogger(this);
	}
	
	@Override
	public boolean handleContent(ContentElement ce, ACLMessage msg) {
		if (ce == null) {
			logger.severe("Message has no content!\n" + msg);
		}
		if (ce instanceof NextRound) {
			
			// Handle next round
			logger.info("INFORM received: next round!");
			
			((IWorldAgent)myAgent).pulse( ((NextRound)ce ).getRoundNumber());
			
			return true;
			
		} else if (ce instanceof IncomingSetting) {
			// TODO: remove? I think this is never used.
			// Do not remove, since Pjotter is using it :)

			// Handle new setting information
			logger.info("INFORM received: new setting information");
			StorySettingElement sse = ((IncomingSetting) ce).getSetting();

			// Just assert
			for (Iterator it = sse.getAllContentTriple(); it.hasNext(); ) {
				RDFtriple t = (RDFtriple) it.next();
				PrologKB.getInstance().tellRDFtriple(t);
				if (! t.getTruth()) {
					logger.warning("Trying to assert a false RDF triple: " + t);
				}
			}
			
			return true;
			
		} else {
			// The agent doesn't know what to do with this INFORM. 
			// If we are sure that this is not supposed to happen, then we don't have to put the message
			// back in the queue (and we can return true), because no other behaviour is going to handle it. 
			// If we are not so sure (maybe there IS another behaviour that handles the 
			// not understood INFORM message), then we should put it back by returning false.
			
			logger.warning("INFORM message not handled: " + ce.toString());
			
			return true;
		}
		
	}

}
