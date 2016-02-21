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

import jade.content.lang.Codec.CodecException;
import jade.content.onto.OntologyException;
import jade.content.onto.basic.Action;
import jade.core.AID;
import jade.core.Agent;
import jade.domain.FIPANames;
import jade.lang.acl.ACLMessage;
import jade.proto.AchieveREInitiator;

import java.util.Vector;
import java.util.logging.Logger;

import vs.IAgent;
import vs.communication.Operator;
import vs.communication.PerformOperator;
import vs.debug.LogFactory;
import vs.plotagent.IPlotAgent;
import vs.rationalagent.RationalAgent;

/**
 * Behaviour for asking the World Agent to perform given operator
 * 
 * @author swartjes
 */
public class InitiateRequestPerformOperatorBehaviour extends
		AchieveREInitiator {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -1682888567186663058L;
	private Logger logger;
	private Operator m_operator;
	
	public InitiateRequestPerformOperatorBehaviour(Agent a, Operator o) {		
		super(a, new ACLMessage(ACLMessage.REQUEST));
		
		logger = LogFactory.getLogger(this);
		m_operator = o;
	}
	
	/**/ 
    @Override
	protected void handleAgree(ACLMessage msg) {
                    logger.info(msg.getSender().getLocalName() +
                    " agreed to perform operator");
    }	
	
    /**/ 
    @Override
	protected void handleInform(ACLMessage msg) {
    	// INFORM is not received for PerformOperator request.
    }

    /**/         
    @Override
	protected void handleRefuse(ACLMessage msg) {
       logger.info( msg.getSender().getLocalName() + " refuses to perform operator.");
    }

    /**
	 * Is called right after the constructor, to prepare the message
	 */
	@Override
	public Vector prepareRequests(ACLMessage msg) {

		((RationalAgent)myAgent ).writeConsole("Starting behaviour: REQUEST perform operator");
		
        try {

        	// Set up a request to perform the action
        	
            msg.setProtocol(FIPANames.InteractionProtocol.FIPA_REQUEST);
    		msg.setLanguage( ((IAgent)myAgent).getCodec().getName());
    		msg.setOntology( ((IAgent)myAgent).getOntology().getName());	
            
            AID worldAgent = ((IPlotAgent)myAgent).getWorldAgent();
    		msg.addReceiver(worldAgent);
            msg.setSender(myAgent.getAID());
            
            PerformOperator po = new PerformOperator();
            po.setOperator(m_operator);
            
            Action a = new Action();
            a.setAction(po);
            a.setActor(worldAgent);
            
            
        	myAgent.getContentManager().fillContent( msg, a );
            // Initiate the request
			
        } catch (CodecException e) {
        	e.printStackTrace();
        } catch (OntologyException e) {
        	e.printStackTrace();
        }
		
		Vector<ACLMessage> v = new Vector<ACLMessage>();
		v.addElement(msg);

		return v;
	}	
	
	

}
