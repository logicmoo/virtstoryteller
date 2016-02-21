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
package vs.characteragent.behaviour;

import jade.content.Concept;
import jade.content.ContentElement;
import jade.content.lang.Codec.CodecException;
import jade.content.onto.OntologyException;
import jade.content.onto.basic.Action;
import jade.content.onto.basic.Result;
import jade.core.Agent;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.proto.SimpleAchieveREResponder;

import java.util.logging.Logger;

import vs.characteragent.ActorProcess;
import vs.characteragent.ICharacterAgent;
import vs.communication.CharacterInfo;
import vs.communication.FramingOperatorPossible;
import vs.communication.GoalSchema;
import vs.communication.PlayCharacter;
import vs.communication.SelectAction;
import vs.communication.StoryAction;
import vs.communication.UseSuggestion;
import vs.debug.LogFactory;

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
		// TODO: more advanced implementation where it checks whether it is a plot agent request 
		// and it is already registered with a Plot agent or not, and whether that Plot agent is 
		// still in the system.
				
		ACLMessage reply = msg.createReply();
		// Assume the worst
		reply.setPerformative(ACLMessage.REFUSE);

		try {
			
			ContentElement ce = null;
			ce = myAgent.getContentManager().extractContent( msg );
			
			Concept actionContent;
			actionContent = ((Action)ce).getAction();
						
			// A request to give control to the Plot Agent
			if (actionContent instanceof PlayCharacter) {

				if (((ICharacterAgent)myAgent).getPlotAgent() == null) {
					// Commit to this Plot agent
					CharacterInfo ci = ((PlayCharacter) actionContent).getCharacterInfo();
					logger.info("I Agree to work for this Plot agent: " + msg.getSender().toString() 
								+ "\nPlaying character: " + ci.getIndividual());
					
					((ICharacterAgent)myAgent).setPlotAgent(msg.getSender());
					((ICharacterAgent)myAgent).setCharacterURI(ci.getIndividual());
					
					reply.setPerformative(ACLMessage.AGREE);
					
				} else if (((ICharacterAgent)myAgent).getPlotAgent().equals(msg.getSender())) {
					// Already working for this plot agent
					logger.info("I already work for this Plot agent: " + msg.getSender().toString()+ " But will agree.");
					((ICharacterAgent)myAgent).setPlotAgent(msg.getSender());
					reply.setPerformative(ACLMessage.AGREE);
					
				} else {
					// Apparantly already working for some other plot agent
					logger.info("I refuse to work for: " + msg.getSender().toString() + " I am already working for: " + ((ICharacterAgent)myAgent).getPlotAgent().toString());
					reply.setPerformative(ACLMessage.REFUSE);			
				}
			
			}
			
			// A request to choose an action
			if (actionContent instanceof SelectAction) {

				// Create action
				StoryAction storyAction = ((ICharacterAgent)myAgent).handleSelectAction();

				if (storyAction == null) {
					// Refuse action selection if select action didn't return any action
					reply.setPerformative(ACLMessage.REFUSE);
				} else {
					// Send the story action.
					reply.setPerformative(ACLMessage.INFORM);
					
					Result result = new Result();				
	
					Action act = new Action();
					act.setAction(actionContent);
					act.setActor(myAgent.getAID());
					result.setAction(act);
								
					result.setValue(storyAction);
					
					logger.info("Sending story action: " + storyAction.getPrologDescription());
					
					myAgent.getContentManager().fillContent( reply, result);
				}
			}
			
			if (actionContent instanceof FramingOperatorPossible) {
				// For now, always agree.
				reply.setPerformative(ACLMessage.AGREE);
			}
			
			// A request to use given suggestion
			if (actionContent instanceof UseSuggestion) {
				
				UseSuggestion us = (UseSuggestion)actionContent;
				
				/* Policy for use of goal schema (Ivo)
				 * ===================================
				 * IF the agent can validate the goal schema, i.e. it believes its preconditions are true,
				 * it will always adopt the goal, overwriting its current intentions. This might not be a permanent
				 * policy, a better one adds the goal to the agents' list of desires, or something.
				 * Current assumption is that the goal schema is "reactive", i.e. something happens that leads to an
				 * immediate change of intentions. 
				 * 
				 * Interesting would be a way to make the preconditions of suggested goal true, i.e. if one needs to 
				 * be a thief in order to adopt a "steal" goal, one can work towards that in earlier actions / improvs.
				 * In terms of story, that might be very interesting.
				 */
				if (us.getSuggestion() instanceof GoalSchema) {
					
					GoalSchema gs = (GoalSchema)us.getSuggestion();
					
					if (( (ICharacterAgent) myAgent).getCharacterProcess() == null) {
						logger.warning("Got a goal suggestion, but I haven't got an actor process.");
						reply.setPerformative(ACLMessage.REFUSE);
					}
					
					if (! (( (ICharacterAgent) myAgent).getCharacterProcess() instanceof ActorProcess)) {
						logger.warning("Actor-level processing is not on.");
						reply.setPerformative(ACLMessage.REFUSE);
					}
					
					ActorProcess actor = (ActorProcess) ( (ICharacterAgent) myAgent).getCharacterProcess();
					
					if ( actor.getDeliberativeLayer().acceptGoalSuggestion(gs) ) {
						
						//((ICharacterAgent) myAgent).sendFabulaElement(gs);
						
						reply.setPerformative(ACLMessage.AGREE);
					} else {
						logger.warning("Cannot add suggested goal: " + gs.getType() + "\nProlog: " + gs.getPrologDescription());
						reply.setPerformative(ACLMessage.REFUSE);
					}
				}


			}			
		
			return reply;

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
	
	/*private boolean validateSchema(String schema) {
		// We got the Prolog head of the schema; validate it.
		// Similar to BasicOperatorSchedule.matchPreconditions; move to common place? (e.g. PrologKnowledgeManager.java)
		Vector<String> vars = new Vector<String>();
		String failedPosPreconditions = "FPP";
		String failedNegPreconditions = "FNP";
		vars.add(failedPosPreconditions);
		vars.add(failedNegPreconditions);
		
		
		Hashtable[] failedPreconditionList;
		failedPreconditionList = PrologKB.getInstance().prologCall(PrologKB.validateOperator, schema, vars);


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
		logger.info("matchPreconditions for schema with head " + schema
				+ ": " + answer);
		return answer;
		
		
		
	}
*/
}
