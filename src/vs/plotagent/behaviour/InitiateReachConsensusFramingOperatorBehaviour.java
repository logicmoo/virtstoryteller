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
import jade.core.behaviours.Behaviour;
import jade.domain.FIPANames;
import jade.lang.acl.ACLMessage;
import jade.proto.ProposeInitiator;

import java.util.Vector;
import java.util.logging.Logger;

import vs.communication.FramingOperator;
import vs.communication.PerformOperator;
import vs.debug.LogFactory;
import vs.plotagent.IPlotAgent;
import vs.rationalagent.RationalAgent;

/**
 * Behaviour for reaching consensus about performing a given framing operator by
 * PROPOSEing to all character agents to execute the framing operator, and allows registering
 * a behaviour to be executed if all ACCEPT-PROPOSAL
 * 
 * @author swartjes
 */
public class InitiateReachConsensusFramingOperatorBehaviour extends
		ProposeInitiator {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 8791119808211493575L;
	private Logger logger;	
	protected FramingOperator framing;
	protected Behaviour allAcceptProposalBehaviour;
	protected Behaviour someRejectProposalBehaviour;
	
	/**
	 * Constructor.
	 *  Expected behaviour upon execution: the behaviour sends an AGREE or REFUSE as a response to
	 *  the original REQUEST
	 *   
	 * @param a the agent that starts the behaviour
	 * @param i the framing operator that the behaviour is trying to reach consensus on
	 */
	public InitiateReachConsensusFramingOperatorBehaviour(Agent a, FramingOperator i) {
		super(a, new ACLMessage(ACLMessage.PROPOSE));
		
		framing = i;
		
		logger = LogFactory.getLogger(this);	
		logger.finer("InitiateReachConsensusFramingOperatorBehaviour created");
	}
	
	/**/ 
    @Override
	protected void handleAllResponses(Vector msgs) {
    	// Agent can perform framing if all agree
    	logger.fine("Handling all responses on proposal");
    	boolean refusal = false;
    	for (Object o: msgs) {
    		ACLMessage msg = (ACLMessage) o;
    		if (msg.getPerformative() != ACLMessage.ACCEPT_PROPOSAL) {
    			logger.warning("Didn't receive an ACCEPT_PROPOSAL: " + msg.toString());
    			refusal = true;
    		}
    		
    	}

    	if (! refusal ) {
    		if (allAcceptProposalBehaviour != null ) {
    	   		logger.info("Adding the behaviour registered for if all accept the proposal.");
    			myAgent.addBehaviour(allAcceptProposalBehaviour);    		
    		} else {
    			logger.warning("No behaviour registered for handling the case where all agree");
    		}    		
    	} else {    		
    		logger.warning("Not all agents accepted the framing operator");
    		if (someRejectProposalBehaviour != null) {
    	   		logger.info("Adding the behaviour registered for if some reject the proposal.");
    			myAgent.addBehaviour(someRejectProposalBehaviour);
    		} else {
    			logger.warning("No behaviour registered for handling the case where some reject");
    		}
    	}
    	
   	}
	
	/**
	 * Is called right after the constructor, to prepare the message
	 */
	@Override
	public Vector prepareInitiations(ACLMessage msg) {
		
		logger.info("Sending framing proposals.");
		((RationalAgent)myAgent ).writeConsole("Starting behaviour: PROPOSE execute framing operator");
		
        msg.setProtocol(FIPANames.InteractionProtocol.FIPA_PROPOSE);
		msg.setLanguage( ((IPlotAgent)myAgent).getCodec().getName());
		msg.setOntology( ((IPlotAgent)myAgent).getOntology().getName());	
		msg.setSender(myAgent.getAID());
		
		// Send to all character agents that are casted (and thus might have consistency issues)

		// NOTE: this creates overhead because the initiating character also gets the request
		//  however, genericness of behaviour is preferred over solving this.
		for (AID receiver: ((IPlotAgent)myAgent).getCharacterManager().getCastedCharacters()) {
				msg.addReceiver(receiver);
				logger.fine("Sending framing proposal to " + receiver);
		}

		PerformOperator po = new PerformOperator();
		po.setOperator(framing);
		Action a = new Action();
		a.setAction(po);
		
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
	
	public void registerHandleAllAcceptProposal(Behaviour b) {
		allAcceptProposalBehaviour = b;
		allAcceptProposalBehaviour.setDataStore(getDataStore());
	}
	
	
    public void registerHandleSomeRejectProposal(Behaviour b) {
		someRejectProposalBehaviour = b;
		someRejectProposalBehaviour.setDataStore(getDataStore());
	}					
                    

}
