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
package vs.poplanner;

import java.util.Vector;
import java.util.logging.Logger;

import vs.debug.LogFactory;
import vs.knowledge.PrologKB;

/**
 * Interface for Partial Order Planner
 * 
 * @author kruizingaEE
 * Created on 3-may-2007
 
*/
public class PoPlanner {

	private final Logger logger;

	private String m_character;
	private String m_Goals;
	private String m_currentPlan;	// Stores the current plan for this PoPlanner. Is set by PoPlanner.plan()
	private Vector<String> m_PlanFirstSteps;

	// Create planner for given character
	public PoPlanner(String character) {
		logger = LogFactory.getLogger(this);
		m_character = character;
	}

	/**
	 * Retrieves executable events
	 * @return the executable events
	 */
	public Vector<String> getExecutableEvents() {
		logger.fine("Retrieving all event Steps from Plan.");
		return PrologKB.getInstance().executableEvents(m_currentPlan);
	}
	
	/**
	 * Retrieves executable framing operators (i.e., framing operators that do not depend on the execution of 
	 * earlier steps in the plan)
	 * @return the executable framing operators
	 */
	public Vector<String> getExecutableFramingOperators() {
		logger.fine("Retrieving all framing operator Steps from Plan.");
		return PrologKB.getInstance().executableFramingOperators(m_currentPlan);
	}
	
	/**
	 * Retrieves executable inference operators (i.e., inference operators that do not depend on the execution of 
	 * earlier steps in the plan)
	 * @return the executable inference operators
	 */
	public Vector<String> getExecutableInferenceOperators() {
		logger.fine("Retrieving all inference operator Steps from Plan.");
		return PrologKB.getInstance().executableInferenceOperators(m_currentPlan);
	}
	
	/**
	 * Retrieves the operators from the plan that are ready for execution (i.e., do not depend on the execution of 
	 * earlier steps in the plan)
	 */
	public Vector<String> getExecutableOperators() {
		logger.fine("Retrieving executable operators from plan.");
		m_PlanFirstSteps = PrologKB.getInstance().executableOperators(m_currentPlan);
		
		if (m_PlanFirstSteps == null) {
			logger.warning("First steps is null.");
		}
		return m_PlanFirstSteps;
	}

	/**
	 * Retrieves all causal links from the plan
	 */
	public Vector<PlanLink> getLinks() {
		logger.fine("Retrieving all Links from Plan.");
		return PrologKB.getInstance().getPlanLinks(m_currentPlan);
	}

	/**
	 * Retrieves all orderings from the plan
	 */
	public Vector<PlanOrdering> getOrderings() {
		logger.fine("Retrieving all Orderings from Plan.");
		return PrologKB.getInstance().getPlanOrderings(m_currentPlan);
	}
	
	/**
	 * Return the plan in SWI-Prolog representation. 
	 */
	public String getPlan() {
		return m_currentPlan;
	}	
	
	/**
	 * Retrieves all steps from the plan
	 */
	public Vector<PlanStep> getSteps() {
		logger.fine("Retrieving all Steps from Plan.");
		return PrologKB.getInstance().getPlanSteps(m_currentPlan);
	}		
	
	/**
	 * Creates and stores a plan for given goals
	 */
	public boolean plan() {
		String newPlan;
		if (m_currentPlan != null ) {
			newPlan = PrologKB.getInstance().adaptPlan(m_character, m_Goals, m_currentPlan);
		} else {
			newPlan = PrologKB.getInstance().plan(m_character, m_Goals);
		}
		if (newPlan != null) {
			logger.fine("Successfully made a plan.");
			m_currentPlan = newPlan;
			return true;
		} else {
			logger.fine("Could not make a plan for goals \n" + m_Goals);
			return false;
		}
	}

	public boolean planFinished() {
		return PrologKB.getInstance().finishedPlan(m_currentPlan);
	}

	/**
	 * Sets the goals for the planner (a Prolog list of conditions)
	 */
	public void setGoals(String goals) {
		logger.fine("Setting planner goal to: " + goals);
		m_Goals = goals;
	}
	
	/**
	 * Explicitly set the plan of this planner (use with care)
	 * @param plan the plan for this planner
	 */
	public void setPlan(String plan) {
		m_currentPlan = plan;
	}
	
}
