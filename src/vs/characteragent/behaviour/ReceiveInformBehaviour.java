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

import jade.content.ContentElement;
import jade.core.Agent;
import jade.lang.acl.ACLMessage;

import java.util.logging.Logger;

import vs.characteragent.ICharacterAgent;
import vs.communication.CharacterInfo;
import vs.communication.FabulaCausality;
import vs.communication.FabulaCausalityDeclaration;
import vs.communication.FabulaElement;
import vs.communication.FabulaElementDeclaration;
import vs.communication.IncomingPerception;
import vs.communication.IncomingSetting;
import vs.communication.NextRound;
import vs.communication.OperatorResult;
import vs.communication.StoryPerception;
import vs.debug.LogFactory;
import vs.rationalagent.behaviour.InformReceiver;

/**
 * Behaviour to receive INFORM messages for the Character Agent
 * 
 * @author kruizinga
 *
 */
public class ReceiveInformBehaviour extends InformReceiver {

	/**
	 * 
	 */
	private static final long serialVersionUID = -3551787954456482184L;
	private Logger logger;
	
	public ReceiveInformBehaviour(Agent a) {
		super(a);

		logger = LogFactory.getLogger(this);
		logger.finer("Creating " + this.getBehaviourName());
	}
	
	@Override
	public boolean handleContent(ContentElement ce, ACLMessage msg) {
		if (ce instanceof CharacterInfo) {
			logger.info("Received INFORM: CharacterInfo");
			((ICharacterAgent)myAgent).handleCharacterInfo((CharacterInfo)ce);
			return true;
		} else if (ce instanceof IncomingPerception) {
			// Handle an incoming perception
			logger.info("Received INFORM: Perception");
			StoryPerception sp = ((IncomingPerception)ce).getPerception();
			((ICharacterAgent)myAgent).handlePerception(sp);
			return true;
		} else if (ce instanceof NextRound) {
			// Handle next round
			logger.info("Received INFORM: NextRound");
			((ICharacterAgent)myAgent).handleNextRound((NextRound)ce);
			return true;
		} else if (ce instanceof IncomingSetting) {
			// Handle next round
			logger.info("Received INFORM: IncomingSetting");
			((ICharacterAgent)myAgent).handleIncomingSetting((IncomingSetting)ce);
			return true;
		} else if (ce instanceof OperatorResult) {
			// Handle result of operator
			logger.info("Received INFORM: OperatorResult");
			((ICharacterAgent)myAgent).handleOperatorResult((OperatorResult)ce);
			return true;
		} else if (ce instanceof FabulaElementDeclaration) {
			logger.info("Received INFORM: FabulaElementDeclaration");
			// TODO: this information should be treated as out of character (OOC)
			FabulaElement fe = ((FabulaElementDeclaration) ce).getFabulaElement();
			((ICharacterAgent) myAgent).getEpisodicMemory().addFabulaElement(fe);
			return true;
		} else if (ce instanceof FabulaCausalityDeclaration) {
			logger.info("Received INFORM: FabulaCausalityDeclaration");
			// TODO: this information should be treated as out of character (OOC)
			FabulaCausality fc = ((FabulaCausalityDeclaration) ce).getFabulaCausality();
			((ICharacterAgent) myAgent).getEpisodicMemory().addFabulaCausality(fc);
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
