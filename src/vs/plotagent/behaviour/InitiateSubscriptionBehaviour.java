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
package vs.plotagent.behaviour;

import jade.core.Agent;
import jade.domain.DFService;
import jade.domain.FIPAException;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.lang.acl.ACLMessage;
import jade.proto.SubscriptionInitiator;

import java.util.logging.Logger;

import vs.debug.LogFactory;
import vs.plotagent.BasicPlotAgent;

/**
 * A behaviour for the Plot Agent to SUBSCRIBE to other agents 
 * entering the MAS. 
 **/
public class InitiateSubscriptionBehaviour extends SubscriptionInitiator
{
	/**
	 * 
	 */
	private static final long serialVersionUID = -3788435051195332300L;
	private static ACLMessage createMessage(String serviceType, Agent a) {
		DFAgentDescription agentDescription = new DFAgentDescription();// fill the template
		ServiceDescription serviceDescription = new ServiceDescription();

		serviceDescription.setType(serviceType);
		agentDescription.addServices(serviceDescription);
		ACLMessage subscriptionMessage = DFService
				.createSubscriptionMessage(a, a.getDefaultDF(),
						agentDescription, null);

		return subscriptionMessage;
	}
	private Logger logger;
	private ACLMessage subscriptionMessage; 
	
	private boolean cancelled = false;
	
	public InitiateSubscriptionBehaviour (Agent a, String serviceType) {
		super(a, createMessage(serviceType, a));

		logger = LogFactory.getLogger(this);
	}
	
	/**
	 * Cancels the subscription
	 */
	public void cancelSubscription() {
		
		/* This cancels a subscription. It takes care of
		 * composing the CANCEL message based on the
		 * original SUBSCRIBE message. If we say false to
		 * ignoring responses then we must handle the
		 * possible handleInform and handleFailure methods
		 * before the behaviour will finish.
		 */
		cancel(myAgent.getDefaultDF(), true);
		cancelled = true;
	}	

	@Override
	protected void handleInform(ACLMessage inform) {
		//logger.info("Subscription received. Passing it on to the agent...");
		
		try {

			logger.info("Subscription message received.");
			DFAgentDescription[] dfds = DFService.decodeNotification(inform.getContent());
			if (dfds.length <1) {
				logger.severe("Directory service of subscription is null!");
			}
			
			// Run through all agent descriptions...
			for (int x = 0; x < dfds.length; x++) {
				logger.finer("Handling subscription #" + x);
							
				((BasicPlotAgent)myAgent).handleSubscription(dfds[x]);
			}
		}
		catch (FIPAException fe) {
			fe.printStackTrace(); 
		}
	}
	
	@Override
	public int onEnd() {
		logger.info(myAgent.getName() + " has finished subscription behaviour.");
		
		if (! cancelled) {
			cancelSubscription();
		}
		
		return super.onEnd();
	}
}