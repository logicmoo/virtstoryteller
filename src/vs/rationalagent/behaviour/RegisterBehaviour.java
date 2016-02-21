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

import jade.core.AID;
import jade.core.Agent;
import jade.core.behaviours.OneShotBehaviour;
import jade.domain.DFService;
import jade.domain.FIPAException;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;

/**
 * A one shot behaviour that registers an agent with the DF
 **/

public class RegisterBehaviour extends OneShotBehaviour
{
	/////////////////////////////////////////////
	// ATTRIBUTES
	/////////////////////////////////////////////
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 7604969547785089939L;
	private String			m_serviceType;
	//private String			m_serviceName;
	
	/////////////////////////////////////////////
	// CONSTRUCTORS AND INSTANTIATION
	/////////////////////////////////////////////
		
	public RegisterBehaviour(Agent a, String serviceType) //, String serviceName)
	{
		super(a);
		m_serviceType = serviceType;
		//m_serviceName = serviceName;
	}

	/////////////////////////////////////////////
	// METHODS
	/////////////////////////////////////////////
	
	@Override
	public void action()
	{
		//Create servicedescription for this agent:
		ServiceDescription sd = new ServiceDescription();
		sd.setType(m_serviceType);
		sd.setName(myAgent.getLocalName());
		//SHOULD ALSO REGISTER ONTOLOGIES AND LANGUAGES
		//Put servicedescription into an appropriate DFAgentDescription:
		DFAgentDescription dfd = new DFAgentDescription();
		dfd.setName(myAgent.getAID());
		dfd.addServices(sd);
		//Register the description with the DF
		AID dfAID = myAgent.getDefaultDF();
		try {
			DFService.register(myAgent, dfAID, dfd);
		} catch( FIPAException e ) {
			e.printStackTrace();
		}
	}
}