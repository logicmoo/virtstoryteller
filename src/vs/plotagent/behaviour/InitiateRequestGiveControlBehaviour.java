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

import vs.communication.GiveControl;
import vs.debug.LogFactory;
import vs.plotagent.IPlotAgent;
import vs.rationalagent.RationalAgent;

/**
 * Behaviour for asking a World Agent to give control over its world state over to the plot agent
 * in other words, the Plot Agent asks permission to use this World Agent to send its actions and events to.
 * 
 * @author swartjes
 */
public class InitiateRequestGiveControlBehaviour extends AchieveREInitiator {
	

	private static final long serialVersionUID = 4575507303497999483L;
	private Logger logger;
	private AID m_receiver;
	
	public InitiateRequestGiveControlBehaviour(Agent a, AID newWorldAgent) {
		super(a, new ACLMessage(ACLMessage.REQUEST));
			
		m_receiver = newWorldAgent;
		
		logger = LogFactory.getLogger(this);
		logger.info("Initiate request to give control behaviour created.");
	}
	
	/**/ 
    @Override
	protected void handleAgree(ACLMessage msg) {
    	logger.info(msg.getSender().getLocalName() + " agrees to be controlled by me");    	
    	
    	// Set world agent if its not already bound
    	if (((IPlotAgent)myAgent).getWorldAgent() == null) {
    		logger.info("Committing to World Agent " + msg.getSender().getLocalName());
    		((IPlotAgent)myAgent).setWorldAgent(msg.getSender());
    	}

    }
	
    /**/ 
    @Override
	protected void handleInform(ACLMessage msg) {
        logger.finer("Initiator: Has received a inform message: "+msg);
    }

    /**/         
    @Override
	protected void handleRefuse(ACLMessage msg) {
       logger.info( msg.getSender().getLocalName() + " refuses to be controlled by me");
    }

    /**
	 * Is called right after the constructor, to prepare the message
	 */
	@Override
	public Vector prepareRequests(ACLMessage msg) {
		logger.info("Preparing REQUEST messages...");
		((RationalAgent)myAgent ).writeConsole("Starting behaviour: REQUEST give control");
        try {
        	// Create REQUEST to GiveControl 
            msg.setProtocol(FIPANames.InteractionProtocol.FIPA_REQUEST);
    		msg.setLanguage( ((IPlotAgent)myAgent).getCodec().getName());
    		msg.setOntology( ((IPlotAgent)myAgent).getOntology().getName());	
            
            msg.addReceiver(m_receiver);
            msg.setSender(myAgent.getAID());

            Action act = new Action(myAgent.getAID(), new GiveControl());
            
            //GiveControl givecontrol = new GiveControl();                
            myAgent.getContentManager().fillContent( msg, act );
            
            // Initiate the request
			
        } catch (CodecException e) {
        	e.printStackTrace();
        } catch (OntologyException e) {
        	e.printStackTrace();
        }
		
		Vector v = new Vector();
		v.addElement(msg);
		logger.finer("REQUEST messages: " + v.toString());
		return v;
	}	
	
	

}
