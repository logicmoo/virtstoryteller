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

import vs.communication.CharacterInfo;
import vs.communication.PlayCharacter;
import vs.debug.LogFactory;
import vs.plotagent.IPlotAgent;
import vs.plotagent.ui.AgentsChangedEvent;
import vs.rationalagent.RationalAgent;

/**
 * Behaviour for asking a character agent if it wants to join the story this plot agent
 * is trying to create.
 *  
 * @author swartjes
 *
 */
public class InitiateRequestPlayCharacterBehaviour extends
		AchieveREInitiator {

	private static final long serialVersionUID = -6003712312447004031L;
	private Logger logger;
	private AID m_receiver;
	private CharacterInfo m_charInfo;
	
	public InitiateRequestPlayCharacterBehaviour(Agent a, AID newCharacterAgent, CharacterInfo info) {
		super(a, new ACLMessage(ACLMessage.REQUEST));
		
		m_receiver = newCharacterAgent;
		m_charInfo = info;
		
		logger = LogFactory.getLogger(this);
	}
	
	/*
     */
    @Override
	protected void handleAgree(ACLMessage msg) {
                    logger.info(msg.getSender().getLocalName() +
                    " wants to join the story!");
                    
                    // Cast definitely.
                    ((IPlotAgent)myAgent).getCharacterManager().castCharacter(msg.getSender(), m_charInfo.getIndividual());
                    ((IPlotAgent)myAgent).fireEvent(new AgentsChangedEvent(this));
                    
    }	
	
    /**/ 
    @Override
	protected void handleInform(ACLMessage msg) {
        // Do nothing
    }

    /**/         
    @Override
	protected void handleRefuse(ACLMessage msg) {
       logger.info( msg.getSender().getLocalName() + " refuses to join the story.");

       // Put back on necessary character queue
       ((IPlotAgent)myAgent).getCharacterManager().addWantedCharacter(m_charInfo.getIndividual());
       
       
    }

    /**
	 * Is called right after the constructor, to prepare the message
	 */
	@Override
	public Vector prepareRequests(ACLMessage msg) {
		((RationalAgent)myAgent ).writeConsole("Starting behaviour: REQUEST join story");

        try {
        	// Create REQUEST to GiveControl 
            msg.setProtocol(FIPANames.InteractionProtocol.FIPA_REQUEST);
    		msg.setLanguage( ((IPlotAgent)myAgent).getCodec().getName());
    		msg.setOntology( ((IPlotAgent)myAgent).getOntology().getName());	
            
            msg.addReceiver(m_receiver);
            msg.setSender(myAgent.getAID());

            PlayCharacter js = new PlayCharacter();
            js.setCharacterInfo(m_charInfo);
            
            Action a = new Action(m_receiver, js);
            //JoinStory joinstory = new JoinStory();                
            myAgent.getContentManager().fillContent( msg, a );
            
            // Initiate the request
			
        } catch (CodecException e) {
        	e.printStackTrace();
        } catch (OntologyException e) {
        	e.printStackTrace();
        }
		
		Vector v = new Vector();
		v.addElement(msg);

		return v;
	}	
	
	

}
