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

import vs.communication.FabulaElement;
import vs.communication.UseSuggestion;
import vs.debug.LogFactory;
import vs.plotagent.IPlotAgent;
import vs.rationalagent.RationalAgent;

public class InitiateRequestUseSuggestionBehaviour extends
		AchieveREInitiator {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 8791119808211493575L;
	private Logger logger;	
	private AID m_character;
	private FabulaElement m_suggestion;
	
	public InitiateRequestUseSuggestionBehaviour(Agent a, AID character, FabulaElement suggestion) {
		super(a, new ACLMessage(ACLMessage.REQUEST));
		
		logger = LogFactory.getLogger(this);	
		logger.finer("InitiateRequestUseSuggestionBehaviour created");
		assert (character != null);
		m_character = character;
		m_suggestion = suggestion;
	}
	
	/**/ 
    @Override
	protected void handleAgree(ACLMessage msg) {
    	logger.info(msg.getSender().getLocalName() + " agreed to use suggestion " + m_suggestion);
    }


    /**/ 
    @Override
	protected void handleInform(ACLMessage msg) {
    	// INFORM is not received for UseSuggestion request.
        
    }

    /**/         
    @Override
	protected void handleRefuse(ACLMessage msg) {
       logger.warning( msg.getSender().getLocalName() + " refuses to use suggestion " + m_suggestion);
    }

    /**
	 * Is called right after the constructor, to prepare the message
	 */
	@Override
	public Vector prepareRequests(ACLMessage msg) {
		
		logger.info("Sending suggestion request.");
		((RationalAgent)myAgent ).writeConsole("Starting behaviour: REQUEST use suggestion");
		
        msg.setProtocol(FIPANames.InteractionProtocol.FIPA_REQUEST);
		msg.setLanguage( ((IPlotAgent)myAgent).getCodec().getName());
		msg.setOntology( ((IPlotAgent)myAgent).getOntology().getName());	
		msg.setSender(myAgent.getAID());
		
		// Send to all character agents
		// (note: this requires the behaviour to be an AchieveREInitiator and NOT
		//  a SimpleAchieveREInitiator!)
		msg.addReceiver(m_character);
		
		UseSuggestion us = new UseSuggestion();
		us.setSuggestion(m_suggestion);
		
		Action a = new Action();
		a.setAction(us);		
		a.setActor(m_character); 
        
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
