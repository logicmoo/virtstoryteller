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

import jade.content.Concept;
import jade.content.ContentElement;
import jade.content.lang.Codec.CodecException;
import jade.content.onto.OntologyException;
import jade.content.onto.basic.Action;
import jade.core.Agent;
import jade.core.behaviours.SequentialBehaviour;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.proto.SimpleAchieveREResponder;

import java.util.logging.Logger;

import vs.communication.FramingOperator;
import vs.communication.InferenceOperator;
import vs.communication.PerformOperator;
import vs.communication.StoryEvent;
import vs.debug.LogFactory;
import vs.knowledge.PrologKB;
import vs.plotagent.IPlotAgent;
import vs.rationalagent.behaviour.SendMessageBehaviour;

public class RequestResponderBehaviour extends SimpleAchieveREResponder {

	/**
	 * 
	 */
	private static final long serialVersionUID = -590992386588474513L;
	private Logger logger;
	
	public RequestResponderBehaviour (Agent a, MessageTemplate mt) {
		super(a, mt);
		
		logger = LogFactory.getLogger(this);
	}
	
	@Override
	protected ACLMessage prepareResponse (ACLMessage msg) {
				
		ACLMessage reply = msg.createReply();
		// Assume the worst
		reply.setPerformative(ACLMessage.REFUSE);

		try {
			
			ContentElement ce = null;
			ce = myAgent.getContentManager().extractContent( msg );
			
			Concept actionContent;
			actionContent = ((Action)ce).getAction();
						
			// A request from a character to perform an operator (probably a FramingOperator)
			if (actionContent instanceof PerformOperator) {

				PerformOperator po = (PerformOperator)actionContent;
				
				if (po.getOperator() instanceof FramingOperator) {
					
					// For now, always agree.
					//reply.setPerformative(ACLMessage.AGREE);
					
					// TODO: check if valid
					logger.warning("No check is being done to ensure that the FramingOperator is possible according to Plot Agent. Might not be a problem though; World Agent will just REFUSE.");
					
					// Do not respond immediately, but dispatch to other behaviour
					reply = null;

					
					// The following constructed behaviour PROPOSEs to all Character Agents whether performing 
					// the framing operator is OK, and if they all ACCEPT-PROPOSAL, it replies to the request with an 
					// AGREE; if one of them REJECT-PROPOSALs, it replies with a REFUSE.
					
					// Construct behaviour for REFUSEing
					ACLMessage msgRefuse = msg.createReply();
					msgRefuse.setPerformative(ACLMessage.REFUSE);									
					SendMessageBehaviour refuse = new SendMessageBehaviour(myAgent, msgRefuse);

					// Construct behaviour for AGREEing and performing the operator
					SequentialBehaviour agreeAndPerform = new SequentialBehaviour();
					ACLMessage msgAgree = msg.createReply();
					msgAgree.setPerformative(ACLMessage.AGREE);
					SendMessageBehaviour agree = new SendMessageBehaviour(myAgent, msgAgree);
					InitiateRequestPerformOperatorBehaviour perform = new InitiateRequestPerformOperatorBehaviour(myAgent, po.getOperator());
					agreeAndPerform.addSubBehaviour(agree);
					agreeAndPerform.addSubBehaviour(perform);
					
					// Construct the AGREE or REFUSE depending on proposal results behaviour

					// No characters to agree, or FO is personal/hidden, just agree.
					if (((IPlotAgent)myAgent).getCharacterManager().getCastedCharacters().isEmpty() ||
							(! PrologKB.getInstance().isFramingScopeAll(po.getOperator().getPrologDescription()))) {
						myAgent.addBehaviour(agreeAndPerform);
					} else {
					// Otherwise, reach consensus
						InitiateReachConsensusFramingOperatorBehaviour proposal = new InitiateReachConsensusFramingOperatorBehaviour(myAgent, (FramingOperator) po.getOperator());
						proposal.registerHandleAllAcceptProposal(agreeAndPerform);
						proposal.registerHandleSomeRejectProposal(refuse);
					
						myAgent.addBehaviour(proposal);
					}
					
				} else if (po.getOperator() instanceof StoryEvent) {
					
					// TODO: check if valid
					logger.warning("No check is being done to ensure that the Event is possible according to Plot Agent. Might not be a problem though; World Agent will just REFUSE.");
					reply.setPerformative(ACLMessage.AGREE);
					
					logger.info("Executing event: " + po.getOperator().getPrologDescription());
					myAgent.addBehaviour(new InitiateRequestPerformOperatorBehaviour(myAgent, po.getOperator()));
				} else if (po.getOperator () instanceof InferenceOperator) {
					// other characters don't need to agree on inference operators, so just agree
					reply.setPerformative(ACLMessage.AGREE);
					myAgent.addBehaviour(new InitiateRequestPerformOperatorBehaviour(myAgent, po.getOperator()));
				}
				
				else {
					logger.warning("Received request to perform operator, but do not know how to handle!\n" + po.getOperator());
				}
			
			}

		} catch ( CodecException ce ) {
			ce.printStackTrace();
		}
		catch ( OntologyException oe ) {
			oe.printStackTrace();
		}	
		
		return reply;
	}
	
	protected ACLMessage prepareResultNotification (ACLMessage msg, ACLMessage msg2) {
		return null;
	}
	
/*	private String schemaToProlog(Schema s) {
		StringBuilder headBuilder = new StringBuilder();
		headBuilder.append('[');
		headBuilder.append("type('").append(s.getType()).append("')");
		for (Iterator it = s.getAllParameter(); it.hasNext();) {
			Parameter p = (Parameter)it.next();
			headBuilder.append(',');
			if (p instanceof Agens) {
				headBuilder.append("agens('").append(p.getValue()).append("')");
			}
			else if (p instanceof Patiens) {
				headBuilder.append("patiens('").append(p.getValue()).append("')");
			}
			else if (p instanceof Target) {
				headBuilder.append("target('").append(p.getValue()).append("')");
			}
			else if (p instanceof Instrument) {
				headBuilder.append("instrument('").append(p.getValue()).append("')");
			} else {
				headBuilder.append("other('").append(p.getValue()).append("')");
			}
		}
		headBuilder.append(']');
		return headBuilder.toString();
	}
*/	
/*	private boolean validateSchema(String prologHead) {
		// We got the Prolog head of the schema; validate it.
		// Similar to BasicOperatorSchedule.matchPreconditions; move to common place? (e.g. PrologKnowledgeManager.java)
		Vector<String> vars = new Vector<String>();
		String failedPosPreconditions = "FPP";
		String failedNegPreconditions = "FNP";
		vars.add(failedPosPreconditions);
		vars.add(failedNegPreconditions);
		
		
		Hashtable[] failedPreconditionList;
		failedPreconditionList = PrologKB.getInstance().prologCall(PrologKB.validateOperator, prologHead, vars);


		// firstElement [] emptyList means no failed preconditions. We have a list with a list in it.
		logger.fine("failedPreconditionList: "
				+ failedPreconditionList.toString());

		boolean answer = true;
		// if ( failedPreconditionList == null) { throw new Exception(P_VALIDATEACTION + "failed to return anything.") };

		if (failedPreconditionList == null) {
			logger.info("failedPreconditionList == null");
		} else {
			logger.info("failedPreconditionList length is: "
					+ failedPreconditionList.length);
			if (failedPreconditionList.length > 0) {
				logger.info("failedPosPreconditions: "
						+ failedPreconditionList[0].get(failedPosPreconditions)
								.toString()
						+ " "
						+ "failedNegPreconditions: "
						+ failedPreconditionList[0].get(failedNegPreconditions)
								.toString());
				answer = answer
						&& (failedPreconditionList[0].get(
								failedPosPreconditions).toString().equals("[]"));
				answer = answer
						&& (failedPreconditionList[0].get(
								failedNegPreconditions).toString().equals("[]"));
			} else {
				// For instance, when preconditions are not given
				// answer = false; // (what does this mean?)
				answer = true;
			}
		}
		logger.info("matchPreconditions for schema with head " + prologHead
				+ ": " + answer);
		return answer;
		
		
		
	}
*/
}
