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

import java.util.LinkedList;
import java.util.List;
import java.util.Vector;
import java.util.logging.Logger;

import vs.communication.Aborted;
import vs.communication.Finished;
import vs.communication.FramingOperator;
import vs.communication.InferenceOperator;
import vs.communication.Operator;
import vs.communication.OperatorResult;
import vs.communication.RDFtriple;
import vs.communication.Scheduled;
import vs.communication.StoryAction;
import vs.communication.StoryEvent;
import vs.debug.LogFactory;
import vs.knowledge.PrologKB;
import vs.rationalagent.behaviour.SendInformBehaviour;
import vs.worldagent.ui.OperatorsChangedEvent;

/**
 * Implements a basic version of the ActionScheduler interface. 
 * @see vs.worldagent.IOperatorScheduler
 * @author swartjes
 * Created on 15-jul-2005
 */
public class BasicOperatorScheduler implements IOperatorScheduler { 

	/** The World agent that owns this module **/
	private final IWorldAgent m_ownerAgent;

	/** Tracks the time step **/
	//private int m_timestep;
	/** Keeps a schedule of actions **/
	private final List<ScheduledOperator> m_operatorSchedule;
	private final List<ScheduledOperator> m_operatorResults;

	private final Logger logger;
	
	/** set to true, the WA will abort actions that "uses" bodies of agents already used by other actions */
	private static final boolean USE_BODY_CONFLICT = true;

	/**
	 * Constructor
	 * @param owner the Agent that owns this ActionScheduler
	 */
	public BasicOperatorScheduler(IWorldAgent owner) {
		m_ownerAgent = owner;
		m_operatorSchedule = new LinkedList<ScheduledOperator>();
		m_operatorResults = new LinkedList<ScheduledOperator>();
		//m_timestep = 0; // Start at t0

		// Initialize logger
		logger = LogFactory.getLogger(this);
	}

	/* See IActionScheduler */
	public void abort(ScheduledOperator sa) {

		//unschedule(sa);

		// No endtime
		
		sa.setStatus(ScheduledOperator.ABORTED);
		m_ownerAgent.fireEvent(new OperatorsChangedEvent(this));
		m_operatorResults.add(sa);

		if (logger.isLoggable(jade.util.Logger.INFO)) {
			logger.log(jade.util.Logger.INFO, "Operator aborted: " + sa.getOperator().getIndividual());
			//logger.info("Action aborted");	
		}

		// Inform Plot Agent
		OperatorResult or = new OperatorResult();
		or.setOperator(sa.getOperator());
		or.setStatus(new Aborted());
		sendResult(or);

	}

	/* See IActionManager */
	public int calcDuration(Operator theOperator) {

		return PrologKB.getInstance().getOperatorDuration(theOperator.getPrologDescription());
	}

	/* See IActionManager */
	public int calcInterruptableDuration(Operator theOperator) {

		return 0;
	}

	/* See IActionScheduler */
	private ScheduledOperator calcSchedule(Operator o) {
		// Start now
		int start = m_ownerAgent.getTime();

		// Inter after duration
		int inter = start
				+ calcInterruptableDuration(o);

		// End after duration
		int end = start + calcDuration(o);
		if (end < inter) {
			end = inter;
		}

		return new ScheduledOperator(o, start, inter, end);
	}

	/**
	 * Removes finished and aborted actions 
	 */
	private void cleanUp() {
		List<ScheduledOperator> trashBin = new LinkedList<ScheduledOperator>();

		for (ScheduledOperator so : m_operatorSchedule) {
			if ((so.getStatus() == ScheduledOperator.ABORTED)
					|| (so.getStatus() == ScheduledOperator.FINISHED)) {
				trashBin.add(so);
			}
		}

		m_operatorSchedule.removeAll(trashBin);
		if (trashBin.size() > 0) {
			m_ownerAgent.fireEvent(new OperatorsChangedEvent(this));
		}
	}

	/* See Interface description */
	public void clearOperatorResults() {
		m_operatorResults.clear();
	}

	/* See Interface description */
	public IWorldAgent getAgent() {
		return m_ownerAgent;
	}

	/* See Interface description */
	public List<ScheduledOperator> getOperatorResults() {
		return m_operatorResults;
	}

	public List<ScheduledOperator> getScheduledOperators() {
		return m_operatorSchedule;
	}

	private boolean matchPreconditions(Operator o) {
		
		return PrologKB.getInstance().checkSchemaFacts(o.getPrologDescription());
	}

	/**
	 * Make a simple narration of operator
	 * @param operator the operator to narrate
	 */
	private void narrate(Operator operator) {
		String narration = PrologKB.getInstance().narrate(operator.getPrologDescription());
		((BasicWorldAgent)m_ownerAgent).narrate(narration);
	}

	private void performOperator(Operator operator) {
		logger.info("Trying to perform operator "
				+ operator.getPrologDescription());
		
		Vector<RDFtriple> effects = null;
		if (PrologKB.getInstance().applyOperatorEffects(operator.getPrologDescription())) {
			effects = PrologKB.getInstance().getOperatorEffects(operator.getPrologDescription());
		}
		
		if (effects != null) {
			narrate(operator);

		} else {
			logger.severe("failed to apply operator effects using applyOperatorEffects.");
		}

	}

	/* See IActionScheduler */
	public void pulse() {

		// Increase time step MOVED TO BasicWorldAgent keeping it central.
		//m_timestep++;

		cleanUp();

		/*
		 * First, handle Events, then handle Actions
		 * Assumption: Event execution always precedes Action execution
		 *  (e.g.: WHILST walking to a place, one can lose an item
		 *         WHILST spinning the wheel, the character pricks herself
		 *         WHILST walking over the bridge, the bridge breaks (and the action fails))
		 */
		for (ScheduledOperator nxtOperator : m_operatorSchedule) {

			// Compare inter time with timestep
			// removed for now.

			// Compare end time with timestep
			if (nxtOperator.getOperator() instanceof StoryEvent) {

				if (m_ownerAgent.getTime() >= nxtOperator.getEndTime()) {

					tryExecute(nxtOperator);

				} else {
					if (m_ownerAgent.getTime() >= nxtOperator.getStartTime()) {
						nxtOperator.setStatus(ScheduledOperator.STARTED);
						m_ownerAgent.fireEvent(new OperatorsChangedEvent(this));
					}
				}
			}
		}

		// Now, handle Actions
		for (ScheduledOperator nxtOperator : m_operatorSchedule) {

			// Compare inter time with timestep
			// removed for now.

			// Compare end time with timestep
			if (nxtOperator.getOperator() instanceof StoryAction) {
				if (m_ownerAgent.getTime() >= nxtOperator.getEndTime()) {

					tryExecute(nxtOperator);

				} else {
					if (m_ownerAgent.getTime() >= nxtOperator.getStartTime()) {
						nxtOperator.setStatus(ScheduledOperator.STARTED);
						m_ownerAgent.fireEvent(new OperatorsChangedEvent(this));
					}
				}
			}
		}

	}
	
	/* See IActionScheduler */
	public boolean schedule(Operator newOperator) {

		logger.info("Scheduling: " + newOperator.getPrologDescription() + " (" + newOperator.getClass() + ")");
		ScheduledOperator newScheduledOperator = calcSchedule(newOperator);
		newScheduledOperator.setStatus(ScheduledOperator.CREATED);
		m_operatorSchedule.add(newScheduledOperator);
		m_ownerAgent.fireEvent(new OperatorsChangedEvent(this));
		
		// Check if preconditions hold
		if (!(matchPreconditions(newOperator))) {

			abort(newScheduledOperator);
			// Inform Plot Agent of failure
			OperatorResult or = new OperatorResult();
			or.setOperator(newOperator);
			or.setStatus(new Aborted());
			sendResult(or);

			String untruePrecs = PrologKB.getInstance().getUntruePreconditionsOfSchema(newOperator.getPrologDescription());
			
			if (logger.isLoggable(jade.util.Logger.INFO)) {
				logger.log(jade.util.Logger.INFO, "(t="
						+ m_ownerAgent.getTime() + ") Scheduling operator "
						+ newOperator.getIndividual()
						+ " refused: preconditions don't hold.\nUntrue preconditions: " + untruePrecs);
			}
			return false;
		} else if (newOperator instanceof FramingOperator) {
			// Execute immediately
			tryExecute(newScheduledOperator);
			return true;
		} else if (newOperator instanceof InferenceOperator) {
			// Execute immediately
			tryExecute(newScheduledOperator);
			return true;
				
		} else {
			
			/*
			 * Consistency: only one action per AGENS is allowed!
			 * Replace possible existing actions for this AGENS
			 * Note: an Event is not an Action and it is possible 
			 * that there is an Event with the same AGENS as an 
			 * already scheduled action. In that case, do NOT replace
			 * the action. 
			 */
			for (Object element : m_operatorSchedule) {
				ScheduledOperator so = (ScheduledOperator) element;

				if ((so.getOperator().getAgens() != null) 
						&& (so != newScheduledOperator) 
						&& (newOperator instanceof StoryAction)
						&& (so.getStatus() != ScheduledOperator.ABORTED)
						&& (so.getStatus() != ScheduledOperator.FINISHED)) {
					
					/* Consistency I: Body already in use!
					 * the assumption is that each action "uses" the bodies of the characters in its arguments.
					 * e.g., Agens give Target to Patiens, gebruikt de bodies van Agens en Patiens
					 * This goes wrong in cases like "someone told Eve that Adam liked her", which doesnt use Adams body
					 */
					if (introducesBodyConflict(newOperator, so.getOperator())) {

					
						// Experiment: abort the OLD action. This way, new actions can "interrupt" old ones
						abort(so);
						//abort(newScheduledOperator);
						
						if (logger.isLoggable(jade.util.Logger.INFO))
							logger.log(jade.util.Logger.INFO, "(t="
									+ m_ownerAgent.getTime() + ") operator "
									+ so.getOperator().getIndividual()
									+ " was aborted because there is a body conflict with a new action ");
						//return false;
					}
				
					// Consistency II: There is already another active action for this agens: replace by the new action
					if (so.getOperator().getAgens().equals(newOperator.getAgens())) {
						abort(so);
						if (logger.isLoggable(jade.util.Logger.INFO))
							logger.log(jade.util.Logger.INFO, "(t="
									+ m_ownerAgent.getTime() + ") operator "
									+ so.getOperator().getIndividual()
									+ " was replaced by "
									+ newOperator.getIndividual() + ".");
						//return false;
					}					

				}
			}

			// Add to schedule
			newScheduledOperator.setStatus(ScheduledOperator.SCHEDULED);
			newOperator.setStarttime(m_ownerAgent.getTime());
			m_ownerAgent.fireEvent(new OperatorsChangedEvent(this));
			
			// Inform Plot Agent
			OperatorResult or = new OperatorResult();
			or.setOperator(newOperator);
			or.setStatus(new Scheduled());
			sendResult(or);			

			if (logger.isLoggable(jade.util.Logger.INFO)) {
				logger.log(jade.util.Logger.INFO, "(t="
						+ m_ownerAgent.getTime() + ") Scheduling operator "
						+ newOperator.getIndividual()
						+ " successful for agens " + newOperator.getAgens() + ".");
			}

			//	logger.info("(t=" + m_timestep + ") Scheduling action " + newAction.name() + " successful for agens " + thisAgens + ".");
			return true;
		}
	}	
	
	// Send an operator result to the plot agent
	private void sendResult(OperatorResult or) {
		
		m_ownerAgent.getAgent().addBehaviour(
				new SendInformBehaviour(m_ownerAgent.getAgent(),
						new AID[] { m_ownerAgent.getPlotAgent() }, or));
	}	
	
	private boolean introducesBodyConflict(Operator op1, Operator op2 ) {
		// TODO: this implementation is a bit too strict. Make sure we're looking at characters (& objects?) only.
		// (this goes wrong for instance if both characters "use" the same location, i.e., both go to a certain location). 
		return USE_BODY_CONFLICT && ( isEqual(op1.getAgens(), op2.getPatiens()) 
		|| isEqual(op1.getAgens(), op2.getTarget())
		|| isEqual(op1.getAgens(), op2.getInstrument()) 
		
		|| isEqual(op1.getPatiens(),  op2.getAgens())
		|| isEqual(op1.getPatiens(),  op2.getPatiens())
		|| isEqual(op1.getPatiens(),  op2.getTarget())
		|| isEqual(op1.getPatiens(),  op2.getInstrument())
		
		|| isEqual(op1.getTarget(),  op2.getAgens())
		|| isEqual(op1.getTarget(),  op2.getPatiens())
		|| isEqual(op1.getTarget(),  op2.getTarget())
		|| isEqual(op1.getTarget(),  op2.getInstrument())
		
		|| isEqual(op1.getInstrument(),  op2.getAgens())
		|| isEqual(op1.getInstrument(),  op2.getPatiens())
		|| isEqual(op1.getInstrument(),  op2.getTarget())
		|| isEqual(op1.getInstrument(),  op2.getInstrument())	);
		
	}
	
	private boolean isEqual(String a, String b) {
		if (a == null || b == null) return false;
		
		return a.equals(b);
	}

	/**
	 * @return String representation
	 */
	@Override
	public String toString() {
		StringBuffer buf = new StringBuffer();

		buf.append("Operator schedule (t=").append(m_ownerAgent.getTime())
				.append("):\n");
		buf
				.append("---------------------------------------------------------\n");

		for (ScheduledOperator nxt : m_operatorSchedule) {
			buf.append(nxt.getOperator().getIndividual()).append('\t').append(
					nxt.getOperator().getType()).append('\t').append("start ")
					.append(nxt.getStartTime()).append("end ").append(
							nxt.getEndTime()).append("\n");
		}

		return buf.toString();
	}

	/**
	 * Tries to execute the operator, aborts if not possible
	 * 
	 * TODO: merge with performOperator
	 * 
	 * @param so the operator to execute
	 */
	private void tryExecute(ScheduledOperator so) {
		if (!matchPreconditions(so.getOperator())) {

			// Nope, abort action
			abort(so);

		} else {

			// Apply effects (cause: this action)		
			performOperator(so.getOperator());
			
			// Flag for deletion
			unschedule(so);
		}
	}

	/* See IActionScheduler */
	public void unschedule(ScheduledOperator sa) {
		// By default it is finished; overwrite in more specific methods
		sa.setStatus(ScheduledOperator.FINISHED);
		sa.getOperator().setEndtime(m_ownerAgent.getTime());
		m_ownerAgent.fireEvent(new OperatorsChangedEvent(this));
		m_operatorResults.add(sa);

		if (logger.isLoggable(jade.util.Logger.INFO)) {
			logger.log(jade.util.Logger.INFO, "Operator removed from schedule: " + sa.getOperator().getIndividual());
		}

		// Inform Plot Agent
		OperatorResult or = new OperatorResult();
		or.setOperator(sa.getOperator());
		or.setStatus(new Finished());
		sendResult(or);

		//logger.info("Action removed from schedule");			
	}

}
