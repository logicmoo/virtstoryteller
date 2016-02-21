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

import jade.core.AID;

import java.util.HashSet;
import java.util.Set;
import java.util.Vector;
import java.util.logging.Logger;

import vs.communication.FabulaCausality;
import vs.communication.FabulaElement;
import vs.communication.GoalProgress;
import vs.communication.GoalSchema;
import vs.communication.IncomingSetting;
import vs.communication.StoryAction;
import vs.communication.StoryOutcome;
import vs.debug.LogFactory;
import vs.fabula.FabulaFactory;
import vs.knowledge.PrologKB;
import vs.knowledge.vocab.Fabula;
import vs.poplanner.PoPlanner;
import vs.rationalagent.behaviour.SendInformBehaviour;
import vs.utils.Chooser;

/**
 * Deliberative process of the character agent.
 * Uses goals and planning for its appraisal, coping and action selection.
 * 
 * A part of goal management is handled by Prolog, because it uses a lot of unification and selection that Java cannot really handle well.
 * The decision which goals can be adopted is handled by Prolog (keeps the goals as entities in Java, for display / debug); Java asks Prolog 
 * which goals can be adopted, and lets Prolog know which goals ARE adopted.
 * 
 * @author swartjes
 *
 */
public class DeliberativeLayer extends BehaviourLayer {

	protected Set<AdoptedGoalSchema> goals;
	protected AdoptedGoalSchema activeGoal;
	
	private Logger logger;
	
	/**
	 * Constructor
	 * 
	 * @param agent the character agent in the story (e.g. ps:leChuck) 
	 */
	public DeliberativeLayer(ICharacterAgent agent) {
		super(agent);
		
		logger = LogFactory.getLogger(this);
		
		goals = new HashSet<AdoptedGoalSchema>();
		
	}
	
	/**
	 * Retrieves the goal that the agent is currently pursuing
	 * @return the goal under pursuit
	 */
	public AdoptedGoalSchema getActiveGoal() {
		return activeGoal;
	}
	
	/**
	 * Retrieves all goals that the agent has adopted
	 * @return a set of adopted goals
	 */
	public Set<AdoptedGoalSchema> getGoals() {
		return goals;
	}
	
	/**
	 * Retrieves the planner currently "in focus" (i.e., the planner of the active goal)
	 * @return the planner of the active goal
	 */
	public PoPlanner getPlanner() {
		if (activeGoal != null) {
			return activeGoal.getPlanner();
		} else {
			return null;
		}
	}
	
	/**
	 * Deliberative appraisal:
	 * - Adopt new goals and select active goal (FearNot! places goal selection/adoption under appraisal too, although it might also
	 *   be seen as coping) 
	 */
	@Override
	public void appraise() {
		logger.info("Deliberative appraisal started.");
		// See which new goals to adopt
		adoptNewGoals();

		// Select which goal should be active
		setActiveGoal(selectActiveGoal());
		
		if (activeGoal == null) {
			logger.info("No more active goals.");
			return;
		}
		
		// Look at failure conditions of goal; if reached, create negative outcome
		if (PrologKB.getInstance().goalFailureConditionsTrue(activeGoal.getGoalSchema().getPrologDescription())) {
			dropGoal(activeGoal, FabulaFactory.Outcome.negative);
			
			// TODO: see which belief elements psi_caused this negative Outcome
			// approach: 
			//		- get failure conditions
			//		- see which belief elements match it
			
			return;
		}

	}
	
	/**
	 * Deliberative coping: 
	 * 	
	 *  - adjust plan currently under consideration
	 */
	@Override
	public void cope() {
		logger.info("Deliberative coping started.");
		
		// Return if there's no goals to pursue
		if (activeGoal == null) {
			return;
		}
		
		// At this point, there is an active goal that is not null, and failure conditions are also not met.
		String oldPlan = getPlanner().getPlan();
		
		// Let's see if we can make a (new) plan
		if (getPlanner().plan()) {
		
			// Now, there are three special cases that should be dealt with: 
			//	1) plan is finished
			//	2) no plan found, but there was one before
			//	3) no plan found, but also never was one
			// in the first two cases, the goal should be dropped (and appraisal should be called again?).
			// in the third case, we can either keep it (waiting for an opportunity) or drop it without a sound.
			// 		this would require lowering its urgency / moving it down somehow? otherwise the agent gets stuck.
			
			// Is plan finished? This means that the success conditions of the goal have been established.
			// Make positive outcome and select another goal to be active
			if (getPlanner().planFinished()) {
			
				logger.warning("Goal state is true; creating positive outcome and selecting a different goal.");
					
				dropGoal(activeGoal, FabulaFactory.Outcome.positive);
				
				// TODO: see which belief elements psi_caused this positive Outcome
				// approach: 
				//		- get success conditions
				//		- see which belief elements match it
				
				// If we can choose another goal, then cope again
				//cope();
			}
			
		} else {
			// We failed to make a plan.
		
			// Was there a plan before? Interpet as failed outcome.
			if (oldPlan != null) {
	
				logger.info("Goal had a plan before; creating negative Outcome");
				
				dropGoal(activeGoal, FabulaFactory.Outcome.negative);
				
				return;
	
			} else {
			
				// Drop goal without a sound
				logger.info("Goal " + activeGoal.getGoalSchema().getType() + " is unplannable, dropping without outcome (should be neutral outcome?)");
				PrologKB.getInstance().dropGoal(activeGoal.getGoalSchema().getPrologDescription());
				goals.remove(activeGoal);
				activeGoal = null;
			}
		}
		
	}
	
	
	/**
	 * Implementation of selectAction() for deliberative layer: plan-based.
	 * selectAction works on the current plan of the agent, established through the deliberative coping process
	 * 
	 * @return one of the executable actions.
	 */
	@Override
	public StoryAction selectAction() {
		logger.info("Selecting action...");
		if (activeGoal == null) {
			logger.warning("No active goal to select an action for.");
			return null;
		}

		// Do we have a successful plan?
		String currentPlan = activeGoal.getPlanner().getPlan();
		
		if (currentPlan != null) {			
			
			// Is it not done, but there are no actions to choose?
			Vector<String> firstActions = activeGoal.getPlanner().getExecutableOperators();
			
			if (firstActions.isEmpty()) {
				// No executable actions in plan
				logger.info("No executable actions for now; rely on further framing");
			} else {
				// Randomly choose one of the first actions
				String chosenAction = Chooser.randomChoice(firstActions);

				StoryAction a = FabulaFactory.createAction(chosenAction, getAgent().getCharacterURI());
				
				// Create motivation causality
				FabulaCausality fc = new FabulaCausality();
				fc.setSubjectIndividual(activeGoal.getGoalSchema().getIndividual());
				fc.setObjectIndividual(a.getIndividual());
				fc.setCausalProperty(Fabula.motivates);
				
				_fabulaCollector.addFabulaElement(a);
				_fabulaCollector.addFabulaCausality(fc);				
				
				return a;
			}
		} 
		return null;
	}	
	

	/**
	 * Finds goal schemas with established preconditions, and adopts instances of them (if possible).
	 * TODO: One idea that arose is to take consistency into consideration here. Not just adopt all goals just because they are possible,
	 * 		 but only those that are "in the (potential) causal chain", i.e., causally connected to goals already in pursuit. This would
	 * 		 prevent the agent from adopting goals even when preconditions are fulfilled, when these preconditions only relate to settings
	 * 		 or fabula elements that are themselves not "in the causal chain" like a deus ex machina event.
	 * 		    This consistency thing is yet another constraint that the agents can use to "contribute to the story" rather than going in 
	 * 		 all kinds of directions. 
	 * 			Do this at the Prolog side!  -- Ivo
	 */
	private void adoptNewGoals() {
		logger.info("Adopting new goals...");
		
		for (String g: PrologKB.getInstance().getPossibleGoals(getAgent().getCharacterURI())) {
			
			GoalSchema gs = FabulaFactory.createGoalSchema(g, getAgent().getCharacterURI());

			if (gs != null) {

				// Adopt the found goal
				adoptGoal(gs);
				
			} else {
				logger.severe("Could not translate prolog string of goal to goal schema: " + g);
			}
			
			// Recursive, to adopt goals that are motivated by goals adopted in this cycle
			adoptNewGoals();
			
		}
		
	}
	

	/**
	 * Adopts given goal if possible (not already adopted, preconditions hold, etc)
	 * 
	 * @param goal the goal to adopt
	 * @return whether the goal was successfully adopted
	 */
	private boolean adoptGoal(GoalSchema goal) {
		
		// Try to adopt. Prolog handle checking if preconditions still hold, and if it was not already adopted.
		if (! PrologKB.getInstance().adoptGoal(goal.getPrologDescription())) {
			logger.warning("Could not adopt goal. Maybe it was already adopted, maybe preconditions no longer hold.\nGoal: " + goal.getPrologDescription());
			return false;
		}
		logger.info("Adopting goal " + goal.getIndividual() + " - "+ goal.getType());
		logger.fine("Schema of adopted goal: " + goal.getPrologDescription());
		
		AdoptedGoalSchema ags = new AdoptedGoalSchema(goal, getAgent().getCharacterURI());	
						
		goals.add(ags);
		
		// Log in fabula
		_fabulaCollector.addFabulaElement(ags.getGoalSchema());

		// Add beliefs enabling the goal
		for (String ena_bel: PrologKB.getInstance().getEnablingFabulaElements(ags.getGoalSchema())) {
		
			logger.info("Enabling fabula elements of schema " + ags.getGoalSchema().getIndividual() + ":\n" + ena_bel);
			FabulaCausality fc = new FabulaCausality();
			fc.setSubjectIndividual(ena_bel);
			fc.setObjectIndividual(ags.getGoalSchema().getIndividual());
			fc.setCausalProperty(Fabula.enables);
			
			_fabulaCollector.addFabulaCausality(fc);
		}
		
		// Add supergoals motivating the goal
		for (String moti_bel: PrologKB.getInstance().getMotivatingFabulaElements(ags.getGoalSchema())) {
			logger.info("Motivating fabula elements of schema " + ags.getGoalSchema().getIndividual() + ":\n" + moti_bel);
			FabulaCausality fc = new FabulaCausality();
			fc.setSubjectIndividual(moti_bel);
			fc.setObjectIndividual(ags.getGoalSchema().getIndividual());
			fc.setCausalProperty(Fabula.motivates);
			
			_fabulaCollector.addFabulaCausality(fc);
		}
		
		// TODO: add causal link from emotions (psi-causes)
		// fabulaCausalities.add(...);
		
		return true;
	}
	
	/**
	 * Drops given goal from the deliberative process, and creates an outcome depending on given value
	 * @param goal the goal to drop
	 * @param outcome the outcome (positive, negative, etc) of the goal
	 */
	private void dropGoal(AdoptedGoalSchema goal, FabulaFactory.Outcome outcome) {
		logger.info("Dropping goal " + goal.getGoalSchema().getType() + " with outcome " + outcome);

		// Prolog side
		PrologKB.getInstance().dropGoal(goal.getGoalSchema().getPrologDescription());
		
		goals.remove(goal);

		// If it is the active goal too, forget it as the active goal.
		if (goal == activeGoal) {
			activeGoal = null;
		}
		StoryOutcome so = FabulaFactory.createStoryOutcome(outcome, getAgent().getCharacterURI());
		so.setResolves(goal.getGoalSchema().getIndividual());
		
		_fabulaCollector.addFabulaElement(so);
		
		
		
		GoalProgress gp = new GoalProgress();
		gp.setCharacter (m_characterAgent.getCharacterURI());
		gp.setGoal (goal.getGoalSchema ().getPrologDescription ());
		
		switch (outcome) {
			case positive: gp.setGoalstatus (GoalProgress.COMPLETED); break;
			case negative: gp.setGoalstatus (GoalProgress.FAILED); break;
			case neutral: gp.setGoalstatus (GoalProgress.UNKNOWN); break;
		}
		
		AID[] receiver = new AID[1];
		receiver[0] = m_characterAgent.getPlotAgent ();
		logger.info("Sending setting to character " + receiver[0]);
		m_characterAgent.getAgent().addBehaviour(new SendInformBehaviour(m_characterAgent.getAgent(), receiver,
				gp));
	}

	/**
	 * Selects the goal that should currently be active, according to some heuristic.
	 * In current implementation, the heuristic is to take the goal with highest urgency. 
	 * 
	 * @return the goal that should be active
	 */
	public AdoptedGoalSchema selectActiveGoal() {
		logger.info("Selecting active goal...");
		Set<AdoptedGoalSchema> goal_options = new HashSet<AdoptedGoalSchema>();
		float curr_urgency = 0;
		AdoptedGoalSchema selected_goal = activeGoal;
		if (selected_goal != null) {
			curr_urgency = activeGoal.getUrgency();
		}
		
		// See if there is a goal that is more urgent
		for (AdoptedGoalSchema ags: goals) {

			// Biggest urgency up till now: this one becomes only option.
			if (ags.getUrgency() > curr_urgency) {
				goal_options.clear();
				// select
				curr_urgency = ags.getUrgency();
				goal_options.add(ags);
			
			// Equal urgency to a goal earlier found, but at least bigger urgency than selected goal: add to list of options.
			} else if (ags.getUrgency() == curr_urgency 
						&& selected_goal != null 
						&& selected_goal.getUrgency() < curr_urgency) {
				goal_options.add(ags);
			}
		}
		
		// We should now have selected the set of goals with biggest urgency; choose randomly.
		if (! goal_options.isEmpty()) {
			return Chooser.randomChoice(goal_options);
		} else {
			return activeGoal;
		}
	}
	
	/**
	 * Replaces the currently active goal by given goal
	 * @param ags the goal that should become active
	 */
	private void setActiveGoal(AdoptedGoalSchema ags) {
		activeGoal = ags;
		if (activeGoal != null) {
			logger.info("Setting active goal to: " + activeGoal.getGoalSchema().getType());
		} else {
			logger.info("Setting active goal to null.");
		}
	}
}
