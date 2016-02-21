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
package vs.characteragent;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import vs.communication.Operator;
import vs.communication.OperatorResult;
import vs.communication.StoryAction;
import vs.debug.LogFactory;
import vs.knowledge.PrologKB;

/**
 * Maintains execution state of actions, events, framing operators, so the agent knows "what it's doing"
 * TODO: move (at least partially) to Prolog (unification creates overhead)
 * @author swartjes
 *
 */
public class ExecutionState {
	
	protected static ExecutionState M_EXECUTIONSTATE = null;
	
	public static ExecutionState getInstance() {
		if (M_EXECUTIONSTATE == null) {
			M_EXECUTIONSTATE = new ExecutionState();
		}
		return M_EXECUTIONSTATE;	
	}
	protected final Logger logger;
	
	/** Contains the set of operators that the agent is CURRENTLY performing. **/
	private Set<Operator> m_performingOperators;
	/** Contains the set of operators that the agent has performed IN THE PAST **/
	private List<Operator> m_executionHistory;
	
	private ExecutionState() {

		// Initialize logger
		logger = LogFactory.getLogger(this);
		m_performingOperators = new HashSet<Operator>();
		m_executionHistory = new ArrayList<Operator>();
	}
	
	public StoryAction currentAction() {
		for (Operator curOp: m_performingOperators) {
			if (curOp instanceof StoryAction) {
				return (StoryAction) curOp;
			}
		}
		return null;
	}
	
	public boolean performingOperator(Operator o) {
		for (Operator curOp: m_performingOperators) {
			// Do comparison
			if (sameOperator(o, curOp)) {
				logger.info("Already performing operator " + o.getType());
				return true;
			} 
		}
		// If we get here, we haven't found the operator in the list of operators under execution
		logger.info("Not yet performing operator " + o.getType());
		return false;
	}	
	
	public Iterator<Operator> performingOperatorIterator() {
		return m_performingOperators.iterator();
	}
	
	public Iterator<Operator> executionHistoryIterator() {
		return m_executionHistory.iterator();
	}
	
	public void registerOperatorResult(OperatorResult or) {
		if (performingOperator(or.getOperator())) {
			// We're done (either successful or failure)
			removeOperator(or.getOperator());
		} else {
			logger.info("Operator result for operator I wasn't performing: \n" + or.getOperator());
		}
	}
	
	public void registerPerformingOperator(Operator o) {
		m_performingOperators.add(o);
	}
	
	private void removeOperator(Operator o) {
		Operator foundOp = null;
		
		// find its equivalent
		for (Operator curOp: m_performingOperators) {
			if (sameOperator(o, curOp)) {
				// remember
				foundOp = curOp;
			}
		}
		if (foundOp != null) {
			logger.info("Removing operator from execution state:\n" + foundOp);
			m_performingOperators.remove(foundOp);
			m_executionHistory.add(foundOp);
		}
	}
	
	private boolean sameOperator(Operator op1, Operator op2) {
		// Do comparison by Prolog string
		if (op1 == null || op2 == null) {
			logger.severe("One of the operators is not specified:\n" + op1 + "\n" + op2);
			return false;
		}
		if (op1.getPrologDescription() == null || op2.getPrologDescription() == null) {
			logger.severe("One of the operators to compare has empty prolog string:\n" + op1 + "\n" + op2);
			return false;
		}
		// TODO: this is probably creating lots of overhead. Move execution state to Prolog side.
		if (PrologKB.getInstance().unifies(op1.getPrologDescription(), op2.getPrologDescription())) {
			logger.fine("Found equality between operators:\n" + op1.getType());
			return true;
		} 
		
		logger.fine("Operators not the same:\n" + op1.getPrologDescription()+"\n" + op2.getPrologDescription());
		return false;
	}

}
