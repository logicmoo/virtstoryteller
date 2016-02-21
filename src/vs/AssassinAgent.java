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
package vs;

import jade.content.AgentAction;
import jade.content.lang.Codec;
import jade.content.lang.sl.SLCodec;
import jade.content.onto.Ontology;
import jade.content.onto.basic.Action;
import jade.core.Agent;
import jade.domain.DFService;
import jade.domain.FIPAException;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.domain.JADEAgentManagement.JADEManagementOntology;
import jade.domain.JADEAgentManagement.KillAgent;
import jade.lang.acl.ACLMessage;

import java.util.HashSet;
import java.util.Set;

import vs.rationalagent.RationalAgent;

/**
 * Agent to kill all other agents. Muhaha. Used by agentlauncher to clean up.
 * @author swartjes
 *
 */
public class AssassinAgent extends Agent {
	
	protected Codec m_codec;
	protected Ontology m_ontology;
	
	void sendRequest(AgentAction action) {

		 ACLMessage request = new ACLMessage(ACLMessage.REQUEST);
		 request.setLanguage(m_codec.getName());
		 request.setOntology(m_ontology.getName());
		 
		 try {
			 Action a = new Action();
			 a.setAction(action);
			 a.setActor(this.getAMS());
			 getContentManager().fillContent(request, a);
			 request.setSender(this.getAID());
			 request.addReceiver(this.getAMS()); 
			 send(request);
		 }
		 catch (Exception ex) { ex.printStackTrace(); }
		 }
	
	 @Override
	protected void setup() {
		
		Set<DFAgentDescription> agents = new HashSet<DFAgentDescription>();
		
		m_codec = new SLCodec();
		m_ontology = JADEManagementOntology.getInstance();
		
		getContentManager().registerLanguage( m_codec );
		getContentManager().registerOntology( m_ontology );
		
		DFAgentDescription dfd = new DFAgentDescription();
		
		ServiceDescription wa = new ServiceDescription();
		wa.setType(RationalAgent.WORLD_SERVICE);
		ServiceDescription pa = new ServiceDescription();
		pa.setType(RationalAgent.PLOT_SERVICE);
		ServiceDescription ca = new ServiceDescription();
		ca.setType(RationalAgent.CHARACTER_SERVICE);
				
	    try {

	        /* 
	        * Alternatively, you could use class jade.domain.RequestFIPAServiceBehaviour which waits 
	        */ 
			dfd.addServices(wa);
	        for (DFAgentDescription foundAgent: DFService.search(this, dfd)) {
	        	agents.add(foundAgent); 
	        }
	        
	        dfd.clearAllServices();
			dfd.addServices(pa);
	        for (DFAgentDescription foundAgent: DFService.search(this, dfd)) {
	        	agents.add(foundAgent); 
	        }
	        
	        dfd.clearAllServices();
			dfd.addServices(ca);
	        for (DFAgentDescription foundAgent: DFService.search(this, dfd)) {
	        	agents.add(foundAgent); 
	        }

	    } catch (FIPAException fe) {

	        System.err.println(fe); 
	        fe.printStackTrace(); 
	        doDelete(); 

	    } 

/*
		
      	try {
            SearchConstraints c = new SearchConstraints();
            c.setMaxResults (new Long(-1));
			agents = AMSService.search( this, new AMSAgentDescription (), c );
		}
		catch (Exception e) {
            System.out.println( "Problem searching AMS: " + e );
            e.printStackTrace();
		}
*/		
		for (DFAgentDescription ag: agents) {
				
			KillAgent ka = new KillAgent();
			ka.setAgent(ag.getName());
			
			System.out.println("Killing agent " + ag.getName());
			sendRequest(ka);
			
/*			Causes an ENORMOUS delay!
 * 
 * 			ACLMessage msg = blockingReceive();
			if (msg != null) {
				System.out.println("Message received:");
				System.out.println(msg);
			}*/
		}
		doDelete();

	}	
}
