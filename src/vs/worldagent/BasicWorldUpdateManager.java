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
package vs.worldagent;

import jade.core.AID;

import java.util.HashMap;
import java.util.Map;
import java.util.Vector;
import java.util.logging.Logger;

import vs.Config;
import vs.communication.FramingOperator;
import vs.communication.InferenceOperator;
import vs.communication.Operator;
import vs.communication.RDFtriple;
import vs.communication.WorldChange;
import vs.debug.LogFactory;
import vs.knowledge.PrologKB;
import vs.rationalagent.behaviour.SendInformBehaviour;

/**
 * Manages world knowledge: - Handles world change requests - Sends world
 * updates
 * 
 * @author swartjes Created on 20-sep-2005
 * @deprecated at 16 june. Not used anymore; all its remaining functionality moved to OperatorScheduler
 */
@Deprecated
public class BasicWorldUpdateManager implements IWorldUpdateManager {

	/** The world agent that owns this module * */
	private final IWorldAgent m_ownerAgent;
	
	/** The identifier used for world changes without cause **/
	public static String NOCAUSE = "Null";

	/**
	 * New knowledge in the form of a Map of Effects that have a Symbol as cause
	 * (ie a certain worldUpdate can be caused by a certain Action or Event. Can
	 * be null if no cause (ie a world update request).
	 */
	private final Map<Vector<RDFtriple>, String> m_worldUpdates = new HashMap<Vector<RDFtriple>, String>();

	private final Logger logger;

	// private Logger logger =
	// jade.util.Logger.getMyLogger(this.getClass().getName());

	/**
	 * Constructor
	 * 
	 * @param owner the World agent that owns this module
	 */
	public BasicWorldUpdateManager(IWorldAgent owner) {
		m_ownerAgent = owner;
		logger = LogFactory.getLogger(this);
	}

	public IWorldAgent getAgent() {
		return m_ownerAgent;
	}


	private void narrate(Operator operator) {
		String narration = PrologKB.getInstance().narrate(operator.getPrologDescription());
		m_ownerAgent.writeGui(narration);
	}

	/*
	 * 
	 * (non-Javadoc)
	 * 
	 * @see vs.worldagent.IWorldUpdateManager#performOperator(vs.communication.Operator)
	 * TODO: Move to OperatorScheduler and remove the whole sending of world updates. Communication 
	 * 		between WA and PA is then purely in terms of OperatorResult. PA will then translate OperatorResult
	 * 		into Perceptions or Settings, depending on the nature of the operator.
	 */
	@Deprecated
	public void performOperator(Operator operator) {
		logger.info("Trying to perform operator "
				+ operator.getPrologDescription());
		
		Vector<RDFtriple> effects = null;
		if (PrologKB.getInstance().applyOperatorEffects(operator.getPrologDescription())) {
			effects = PrologKB.getInstance().getOperatorEffects(operator.getPrologDescription());
		} 

		if (effects != null) {
		/*if (m_ownerAgent.getKnowledgeManager().call(
				PrologKB.applyOperatorEffects, operator.getPrologDescription())) {*/

			narrate(operator);

			logger.info("Calling sendWorldUpdates with: " + effects.toString());

			// Tell world update manager about world updates, and their cause
			// (namely, the action)
			String cause;
			if (operator instanceof FramingOperator || operator instanceof InferenceOperator) {
				cause = NOCAUSE; 
			} else {
				cause = operator.getIndividual();
			}
			
			sendWorldUpdates(effects, cause);

		} else {
			logger.severe("failed to apply operator effects using applyOperatorEffects.");
		}

	}

	/* See IWorldUpdateManager */
	@Deprecated
	private void sendWorldUpdates(Vector<RDFtriple> updates, String cause) {
		// Store as knowledge updates
		if ((updates != null) && !(updates.isEmpty())) {

			logger.info("Processing world updates caused by " + cause + ": "
					+ updates);

			// Alternative 1: Store them for active retrieval by Plot Agent
			// m_worldUpdates.put(updates, cause);

			// Alternative 2: Send them directly to Plot Agent
			AID[] rec = new AID[] { m_ownerAgent.getPlotAgent() };

			for (RDFtriple t : updates) {
				WorldChange wc = new WorldChange();
				wc.addContentTriple(t);
				if (cause != null) {
					wc.setCause(cause);
				} else {
					wc.setCause(NOCAUSE); // TESTING PURPOSES
				}

				// Truth value no longer needed: is contained within RDFtriple
				// wc.setTruth(e.getOperator() == Effect.ADD);

				m_ownerAgent.getAgent().addBehaviour(
						new SendInformBehaviour(m_ownerAgent.getAgent(), rec,
								wc));
			}
		}
	}
}
