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

import vs.communication.GoalSchema;
import vs.knowledge.PrologKB;
import vs.poplanner.PoPlanner;

/**
 * Wrapper for GoalSchema when it is adopted by the Character agent: it gets a planner
 * 
 * @author swartjes
 *
 */
public class JustifiableGoalSchema {

	// Pjotter: Modified to include steps

	private GoalSchema m_goalSchema;
	protected String m_framingIntentions;
	protected PoPlanner m_planner;
	protected boolean m_suggested;

		
	/**
	 * Creates a new AdoptedGoalSchema
	 * @param gs the goal schema
	 */
	public JustifiableGoalSchema(GoalSchema gs, String characterURI, String plan) {
			
		m_goalSchema = gs;
		m_planner = new PoPlanner(null);
		m_planner.setGoals(	PrologKB.getInstance().getSchemaPreconditions(gs.getPrologDescription()));
		m_planner.setPlan(plan);
	}

	
	/**
	 * Returns the goal schema
	 * @return the goal schema
	 */
	public GoalSchema getGoalSchema() {
		return m_goalSchema;
	}
			
	public PoPlanner getJustifyingPlanner() {
		return m_planner;
	}
	
	/**
	 * Set whether this goal was suggested by plot agent
	 * @param suggested whether this was the case
	 */
	public void setSuggested(boolean suggested) {
		m_suggested = suggested;
	}
	
	/**
	 * Retrieve whether this goal was suggested by plot agent
	 * @return whether this goal was suggested by plot agent
	 */
	public boolean getSuggested() {
		return m_suggested;
	}	
	
}
