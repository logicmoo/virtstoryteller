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
public class AdoptedGoalSchema {

	private GoalSchema m_goalSchema;
	protected String m_intentions;
	protected PoPlanner m_planner;
	protected float m_urgency;
	protected boolean m_suggested;
	
	// The slight increase in urgency for goals that were suggested by the plot agent. 
	// Gives it a selection advantage over goals that are equally urgent but not suggested. 
	protected static float suggestedUrgencyDelta = 0.01f;
		
	/**
	 * Creates a new AdoptedGoalSchema
	 * @param gs the goal schema
	 */
	public AdoptedGoalSchema(GoalSchema gs, String characterURI) {
			
		m_goalSchema = gs;
		m_planner = new PoPlanner(characterURI);
		m_planner.setGoals(	PrologKB.getInstance().goalIntention(gs.getPrologDescription())); 
		m_urgency = PrologKB.getInstance().getGoalUrgency(gs.getPrologDescription());
		m_suggested = PrologKB.getInstance().isSuggestedGoal(gs.getPrologDescription());
	}

	
	/**
	 * Returns the goal schema
	 * @return the goal schema
	 */
	public GoalSchema getGoalSchema() {
		return m_goalSchema;
	}
			
	/**
	 * Returns the partial order planner associated with this goal
	 * @return the planner for this goal
	 */
	public PoPlanner getPlanner() {
		return m_planner;
	}
	
	/**
	 * Return the urgency of the goal (i.e. how immediate should it be executed)
	 * @return urgency (between 0 and 1)
	 */
	public float getUrgency() {
		// Slightly increase urgency when the goal was suggested.
		if (m_suggested) {
			return m_urgency + suggestedUrgencyDelta;
		}
		return m_urgency;
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
