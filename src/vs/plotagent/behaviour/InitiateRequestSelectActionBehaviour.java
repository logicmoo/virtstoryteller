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

import jade.content.ContentElement;
import jade.content.lang.Codec.CodecException;
import jade.content.onto.OntologyException;
import jade.content.onto.basic.Action;
import jade.content.onto.basic.Result;
import jade.core.AID;
import jade.core.Agent;
import jade.domain.FIPANames;
import jade.lang.acl.ACLMessage;
import jade.proto.AchieveREInitiator;

import java.util.Vector;
import java.util.logging.Logger;

import vs.communication.SelectAction;
import vs.communication.StoryAction;
import vs.debug.LogFactory;
import vs.plotagent.BasicPlotAgent;
import vs.plotagent.IPlotAgent;
import vs.rationalagent.RationalAgent;

/**
 * Behaviour for asking all Character Agents to select the next action to perform
 * NOTE: this might have to be changed to a behaviour that asks a GIVEN character agent
 * to select an Action.
 * 
 * @author swartjes
 */
public class InitiateRequestSelectActionBehaviour extends
		AchieveREInitiator {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 8791119808211493575L;
	private Logger logger;	
	private AID m_character;
	
	public InitiateRequestSelectActionBehaviour(Agent a, AID character) {
		super(a, new ACLMessage(ACLMessage.REQUEST));
		
		m_character = character;
		
		logger = LogFactory.getLogger(this);	
		logger.finer("InitiateRequestSelectActionBehaviour created");
	}
	
	/**/ 
    @Override
	protected void handleAgree(ACLMessage msg) {
                    logger.info(msg.getSender().getLocalName() +
                    " agreed to think of an action");
    }
	
	
    /**/ 
    @Override
	protected void handleInform(ACLMessage msg) {
        logger.info("Initiator: Has received a inform message: " + msg);
		try {
			
			ContentElement ce = null;
			ce = myAgent.getContentManager().extractContent( msg );
		
        	// Handle an incoming action: pass it on
			Result result = (Result)ce;
        	
			((BasicPlotAgent)myAgent).handleIncomingAction((StoryAction)result.getValue());
			
			// Log to fabula
			// TODO: this should move to the character agent since the character agent invents the name for the action.
			((BasicPlotAgent)myAgent).getFabulaBuilder().addFabulaElement((StoryAction)result.getValue());
        	
		} catch ( CodecException ce ) {
			ce.printStackTrace();
		}
		catch ( OntologyException oe ) {
			oe.printStackTrace();
		}	
        
    }

    /**/         
    @Override
	protected void handleRefuse(ACLMessage msg) {
       logger.info( msg.getSender().getLocalName() + " refuses to think of an action.");
    }

    /**
	 * Is called right after the constructor, to prepare the message
	 */
	@Override
	public Vector prepareRequests(ACLMessage msg) {
		
		logger.info("Sending action requests.");
		((RationalAgent)myAgent ).writeConsole("Starting behaviour: REQUEST select action");
		
        msg.setProtocol(FIPANames.InteractionProtocol.FIPA_REQUEST);
		msg.setLanguage( ((IPlotAgent)myAgent).getCodec().getName());
		msg.setOntology( ((IPlotAgent)myAgent).getOntology().getName());	
		msg.setSender(myAgent.getAID());
		
		logger.fine("Sending action request to " + m_character);
		msg.addReceiver(m_character);

		Action a = new Action();
		a.setAction(new SelectAction());
		
		// We do not actually use this field but it needs a value. 
		// Semantics: setActor(<agent that should execute the action>)
		// We use: setActor(<doesn't matter>).
		a.setActor(myAgent.getAID()); 
        
        try {
        	myAgent.getContentManager().fillContent( msg, a);
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
