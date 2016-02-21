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
import jade.core.AID;
import jade.core.Agent;
import jade.lang.acl.ACLMessage;

import java.util.Vector;
import java.util.logging.Logger;

import vs.communication.Aborted;
import vs.communication.FabulaCausality;
import vs.communication.FabulaCausalityDeclaration;
import vs.communication.FabulaElement;
import vs.communication.FabulaElementDeclaration;
import vs.communication.Finished;
import vs.communication.GoalProgress;
import vs.communication.OperatorResult;
import vs.debug.LogFactory;
import vs.knowledge.PrologKB;
import vs.plotagent.BasicPlotAgent;
import vs.plotagent.Goal;
import vs.plotagent.IPlotAgent;
import vs.rationalagent.behaviour.InformReceiver;
import vs.rationalagent.behaviour.SendInformBehaviour;

/**
 * Behaviour to receive INFORM messages for the Plot Agent
 * 
 * @author swartjes
 * 
 */
public class ReceiveInformBehaviour extends InformReceiver {

	private static final long serialVersionUID = -7658693527371381508L;
	private final Logger logger;

	public ReceiveInformBehaviour(Agent a) {
		super(a);
		logger = LogFactory.getLogger(this);
	}

	@Override
	/**
	 * Messages handled by this behaviour: - World Agent that sends WorldChange
	 */
	public boolean handleContent(ContentElement ce, ACLMessage msg) {

		/*if (ce instanceof WorldChange) {
			logger.info("WorldChange received.");
			handleWorldChange((WorldChange) ce);
			return true;

		} else */
		
		if (ce instanceof OperatorResult) {

			// Handle result of an operator, executed by the world agent
			logger.info("OperatorResult received.");
			handleOperatorResult((OperatorResult) ce);
			return true;

		} else if (ce instanceof GoalProgress) {
			GoalProgress gp = ((GoalProgress) ce);
			Goal goal = new Goal (gp.getGoal (), gp.getCharacter ());
			logger.info ("Received goal progress on goal " + goal + " with status " + gp.getGoalstatus ());
			Vector<Goal> goals = ((IPlotAgent) myAgent).getThreadManager ().getCurrentThread ().getGoals ();
			if (goals.contains (goal)) {
				logger.info("Goal also contained in thread");
				switch (gp.getGoalstatus ()) {
					case GoalProgress.COMPLETED:((IPlotAgent) myAgent).getThreadManager ().getCurrentThread ().finishGoal (goal); 
				}
			}

/*			Vector<String> chars = ((IPlotAgent) myAgent).getThreadManager ().getCurrentThread ().getCharacters ();
			for (String ch : chars)
				logger.info ("Character: " + ch);
			if (chars.contains (gp.getCharacter ()))
				logger.info ("Character exists in thread");
			Vector<Goal> goals = ((IPlotAgent) myAgent).getThreadManager ().getCurrentThread ().getGoals (gp.getCharacter ());
			if (goals != null) {
				for (Goal g : goals)
					logger.info ("Goal: " + g);
				if(goals.contains (goal)) {
					logger.info("Goal also contained in thread");
					switch (gp.getGoalstatus ()) {
						case GoalProgress.COMPLETED:((IPlotAgent) myAgent).getThreadManager ().getCurrentThread ().finishGoal (goal); 
					}
				}
			}*/
			
			
			return true;
		} else if (ce instanceof FabulaElementDeclaration) {

			// Handle incoming fabula element
			logger.info("FabulaElementDeclaration received.");
			FabulaElement fe = ((FabulaElementDeclaration) ce)
					.getFabulaElement();
			((IPlotAgent) myAgent).getFabulaBuilder().addFabulaElement(fe);
			return true;

		} else if (ce instanceof FabulaCausalityDeclaration) {

			// Handle incoming fabula causality link
			logger.info("FabulaCausalityDeclaration received.");
			FabulaCausality fc = ((FabulaCausalityDeclaration) ce)
					.getFabulaCausality();
			((IPlotAgent) myAgent).getFabulaBuilder().addFabulaCausality(fc);

			return true;

		} else {
			// The agent doesn't know what to do with this INFORM.
			// If we are sure that this is not supposed to happen, then we don't
			// have to put the message
			// back in the queue (and we can return true), because no other
			// behaviour is going to handle it.
			// If we are not so sure (maybe there IS another behaviour that
			// handles the
			// not understood INFORM message), then we should put it back by
			// returning false.

			// Current status (11-5-07): this behaviour listens to ALL INFORMs,
			// also the ones belonging to
			// another behaviour's protocol. So put it back on the queue.

			logger.info("INFORM message not handled by this behaviour (putting back on queue): "
							+ ce.toString());

			return false;
		}

	}

	/***************************************************************************
	 * HANDLERS for the different incoming messages
	 **************************************************************************/


	private void handleOperatorResult(OperatorResult or) {
		// If the operator was scheduled, log startime to the fabula
		// TODO: all agents on same location should be INFORMed of the result of
		// actions?

		logger.info("Handling OperatorResult from World Agent: " + or);
			
		// Execute operator (so Plot Agent is aware of changes)
		if (or.getStatus() instanceof Finished) {
			PrologKB.getInstance().applyOperatorEffects(or.getOperator().getPrologDescription());

			// See if the PlotGoalManager can perform a new framing
			((IPlotAgent) myAgent).getPlotGoalManager().performSteps(false);
		}
		
		// Pass on to perception manager
		((BasicPlotAgent) myAgent).getPerceptionManager()
				.registerOperatorResult(or);

		// Send perceptions and settings to characters
		((BasicPlotAgent) myAgent).getPerceptionManager()
				.informCharacters();
		
		// Finally: pass on to Character Agents
		// NOTE: it is important that this is done last, because otherwise the character thinks the action is finished,
		// 		 but didn't get any perception updates . Result is that he will try the action again.
		if (or.getStatus() instanceof Finished || or.getStatus() instanceof Aborted) {
			for (AID character : ((IPlotAgent) myAgent).getCharacterManager()
					.getCastedCharacters()) {
				
				// TODO: log Setting / State.
				AID[] receiver = new AID[1];
				receiver[0] = character;
				logger.info("Sending operator result to character " + receiver[0]);
				myAgent.addBehaviour(new SendInformBehaviour(myAgent, receiver,
						or));
			}

		}		
		
		
	}
}
