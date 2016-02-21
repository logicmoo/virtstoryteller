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

import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.logging.Logger;

import vs.characteragent.behaviour.InitiateRequestPlotPerformOperatorBehaviour;
import vs.communication.FramingOperator;
import vs.communication.GoalSchema;
import vs.communication.InferenceOperator;
import vs.communication.Operator;
import vs.communication.StoryAction;
import vs.communication.StoryEvent;
import vs.debug.LogFactory;
import vs.fabula.FabulaFactory;
import vs.knowledge.PrologKB;
import vs.poplanner.PlanStep;
import vs.poplanner.PoPlanner;
import vs.rationalagent.StoryTime;
import vs.utils.Chooser;
import vs.utils.UniqueId;

public class ActorDeliberativeLayer extends DeliberativeLayer {
	
	protected Set<JustifiableGoalSchema> justifiableGoals;
	protected JustifiableGoalSchema activeJustifiableGoal;	
	protected int notDoingAnything;
	protected int successfulJustificationRound; // In which round was the last successful justification?
	protected Set<String> eventBuffer;
	
	private Logger logger;
	
	/**
	 * Constructor
	 * @param agent owner of this layer
	 */
	public ActorDeliberativeLayer(ICharacterAgent agent) {
		super(agent);
		logger = LogFactory.getLogger(this);
		
		justifiableGoals = new HashSet<JustifiableGoalSchema>();
		successfulJustificationRound = 0;
		notDoingAnything = 0;
		eventBuffer = new HashSet<String>();

	}
	
	@Override
	public void cope() {
		super.cope();
		
		
		// Only direct when there are no framing operators CURRENTLY in execution. 
		// Reason: partial SettingElements received from framing operators can disrupt the planning process.
		//		   so we need to make sure all setting elements have been received.
		//	       this is by definition when the operator results of the framing operators have been received. 
		boolean framingFree = true;
		for (Iterator<Operator> it = ExecutionState.getInstance().performingOperatorIterator(); it.hasNext(); ) {
			Operator op = it.next();
			if (op instanceof FramingOperator) {
				framingFree = false;
			}
		}

		if (framingFree) {
			direct();
		}
	}
	
	/**
	 * Adds a goal suggestion to the director process. The director will try to "use" the goal. 
	 * @param gs goal schema that was suggested
	 * @return whether the suggestion could be added
	 */
	public boolean acceptGoalSuggestion(GoalSchema gs) {
		
		// move to Prolog.

		/* (1) is goal already adopted? Mark it as "suggested"
		 * (2) else, do goal preconditions hold? Adopt goal, go to (1)
		 * (3) else, can we justify goal preconditions? Adopt justifiable goal, go to (1)
		 * 
		 * TODO: a justifiable goal marked as "suggested" will not be marked as "suggested" when finally adopted as normal goal.
		 */
		
		/*
		 * Goal is suggestable when
		*/
		if (getActiveGoal() != null) {
			// It is already the active goal
			if (PrologKB.getInstance().unifies(
					getActiveGoal().getGoalSchema().getPrologDescription(),
					gs.getPrologDescription())) {
				logger.info("Accepting goal suggestion: was already my active goal.\n" + gs.getType());
				
				return PrologKB.getInstance().suggestGoal(gs.getPrologDescription());
			}	
		}
		
		// it is more important/urgent as active goal or active goal is almost done and then this one will be amongst most important/urgent
		AdoptedGoalSchema hypotheticActiveGoal = selectActiveGoal();
		AdoptedGoalSchema hypotheticNewGoal = new AdoptedGoalSchema(gs, getAgent().getCharacterURI());
		
		if (hypotheticActiveGoal != null) {
			// Less urgent than goal that is / would be active at the moment? Fail.
			if (hypotheticNewGoal.getUrgency() < hypotheticActiveGoal.getUrgency()) {
			logger.info("Refusing goal suggestion: suggested goal is not important enough.\n" + gs.getType());
			return false;
			}
		
		// More than one step to go? Fail.
			Vector<PlanStep> steps = hypotheticActiveGoal.getPlanner().getSteps();
			if (steps != null && steps.size() > 1) {
				logger.info("Refusing goal suggestion: takes too long before my current goal is finished, or don't even have a plan yet.\n" + gs.getType());
				return false;
			}
		}			
		
		// Already adopted
		if (PrologKB.getInstance().isAdoptedGoal(gs.getPrologDescription())) {
			for (AdoptedGoalSchema ags: getGoals()) {
				if (PrologKB.getInstance().unifies(
					ags.getGoalSchema().getPrologDescription(),
					gs.getPrologDescription())) {
					ags.setSuggested(true);
					logger.info("Accepting goal suggestion: I had already adopted it and it is now marked as suggested.\n" + gs.getType());
					return PrologKB.getInstance().suggestGoal(gs.getPrologDescription());
				}
			}
		}
		
		// Not adopted, but adopted as justifiable goal
		if (PrologKB.getInstance().isAdoptedJustifiableGoal(gs.getPrologDescription())) {
			for (JustifiableGoalSchema jgs: getJustifiableGoals()) {
				if (PrologKB.getInstance().unifies(
					jgs.getGoalSchema().getPrologDescription(),
					gs.getPrologDescription())) {
					jgs.setSuggested(true);
					logger.info("Accepting goal suggestion: I had already adopted it to justify and it is now marked as suggested.\n" + gs.getType());
					return PrologKB.getInstance().suggestGoal(gs.getPrologDescription());
				}
			}
		}

		// Not active, not adopted, not adopted as justifiable goal but we can justify it
		String plan = PrologKB.getInstance().getGoalPossibleAfterPlan(m_characterAgent.getCharacterURI(), gs.getPrologDescription());
		if (! plan.equals("")) {
			JustifiableGoalSchema sug = addJustifiableGoal(gs, plan);
			sug.setSuggested(true);
			logger.info("Accepting goal suggestion: I can and will justify it.\n" + gs.getType());
			return PrologKB.getInstance().suggestGoal(gs.getPrologDescription());
		}
		
		return false;
	}
	
	/**
	 * Returns the goals that the layer wants to justify
	 * @return a set of goals that can be justified
	 */
	public Set<JustifiableGoalSchema> getJustifiableGoals() {
		return justifiableGoals;
	}
	
	/**
	 * Returns the goal that is currently actively being justified.
	 * @return the active goal to be justified
	 */
	public JustifiableGoalSchema getActiveJustifiableGoal() {
		return activeJustifiableGoal;
	}
	
	/**
	 * Executes framing operators and events used in the plan, to enable its actions
	 * Move to selectAction?
	 * 
	 * @param planner the planner containing a plan that needs to be framed.
	 */
	private void framePlan(PoPlanner planner) {	
		logger.info("Framing plan.");
		
		Vector<String> framingOperators = planner.getExecutableFramingOperators();
		Vector<String> inferenceOperators = planner.getExecutableInferenceOperators();
		eventBuffer.addAll(planner.getExecutableEvents());
		
		// (1) execute the framing operators that are ready for execution 
		for (String inferenceOp: inferenceOperators) {
			InferenceOperator io = new InferenceOperator();
			
			// To know who invented this operator
			io.setCharacter(m_characterAgent.getCharacterURI()); 
			// Below info is not really needed but might be useful for debugging purposes.
			io.setType(PrologKB.getInstance().getSchemaType(inferenceOp));
			io.setIndividual(UniqueId.generateUniqueIndividual("Inference",
					PrologKB.fromNrSign(m_characterAgent.getCharacterURI())));
			io.setPrologDescription(inferenceOp);
			
			// Ask Plot Agent to perform it!
			if (! ExecutionState.getInstance().performingOperator(io)) {
				m_characterAgent.writeGui("Executing inference operator: " + io.getType());
				logger.info("Executing inference operator: " + io.getType());
				ExecutionState.getInstance().registerPerformingOperator(io);
				m_characterAgent.getAgent().addBehaviour(new InitiateRequestPlotPerformOperatorBehaviour(m_characterAgent.getAgent(), io));
			}
		}
		
		// (2) execute the framing operators that are ready for execution 
		for (String framingOp: framingOperators) {
			FramingOperator fo = new FramingOperator();
			
			// To know who invented this operator
			fo.setCharacter(m_characterAgent.getCharacterURI()); 
			// Below info is not really needed but might be useful for debugging purposes.
			fo.setType(PrologKB.getInstance().getSchemaType(framingOp));
			fo.setIndividual(UniqueId.generateUniqueIndividual("Framing",
					PrologKB.fromNrSign(m_characterAgent.getCharacterURI())));
			fo.setPrologDescription(framingOp);
			
			// Ask Plot Agent to perform it!
			if (! ExecutionState.getInstance().performingOperator(fo)) {
				m_characterAgent.writeGui("Executing framing operator: " + fo.getType());
				logger.info("Executing framing operator: " + fo.getType());
				ExecutionState.getInstance().registerPerformingOperator(fo);
				m_characterAgent.getAgent().addBehaviour(new InitiateRequestPlotPerformOperatorBehaviour(m_characterAgent.getAgent(), fo));
			}
		}
	
	}	
	
	/**
	 * Adds a goal that can be justified, in other words, a plan can be made (of framing operators and events) whose execution 
	 * would enable the preconditions of this goal
	 *  
	 * @param frameableGoal the goal that can be justified
	 * @param plan the plan necessary to justify it (fulfill its preconditions)
	 * 
	 * @return whether adding the goal succeeded
	 */
	private JustifiableGoalSchema addJustifiableGoal(GoalSchema frameableGoal, String plan) {
		logger.info("Adding justifiable goal: " + frameableGoal.getType());
		
		JustifiableGoalSchema fgs = new JustifiableGoalSchema(frameableGoal, m_characterAgent.getCharacterURI(), plan);
		justifiableGoals.add(fgs);
		
		return fgs;
		
	}	
	
	/**
	 * Checks which goals can be justified using a plan of framing operators and events that establish their preconditions. 
	 * Adopts goals that have this property and are also new goals (i.e., not goals already under pursuit).
	 */
	private void adoptJustifiableGoals() {
		logger.info("Adopting goals by justification...");
		
		Map<String, String> goalPlanMap = PrologKB.getInstance().getGoalsPossibleAfterPlan(m_characterAgent.getCharacterURI());
		
		// Check whether found justifiable goals are new in every respect, both in-character and out-of-character
		for (String g: goalPlanMap.keySet()) {

			GoalSchema gs = FabulaFactory.createGoalSchema(g, m_characterAgent.getCharacterURI());
			if (gs == null) {
				logger.severe("Could not translate prolog string of goal to goal schema: " + g);
			} else {
			
				// All is well; add it as a justifiable goal
				addJustifiableGoal(gs, goalPlanMap.get(g));
			}
		}
	}	
	
	/**
	 * Selects a justifiable goal to become active.
	 * @return active justifiable goal.
	 */
	private JustifiableGoalSchema selectActiveJustifiableGoal() {
		logger.info("Selecting active justifiable goal");
		// Precedence to suggested goals
		Set<JustifiableGoalSchema> suggested = new HashSet<JustifiableGoalSchema>();
		for (JustifiableGoalSchema j: justifiableGoals) {
			if (j.getSuggested()) {
				logger.fine("Goal was suggested: " + j.getGoalSchema().getType() );
				suggested.add(j);
			}
		}	
		
		if (! suggested.isEmpty()) {
			return Chooser.randomChoice(suggested);
		}
		
		// Regular selection
		if (activeJustifiableGoal == null) {
			// Randomly select new, but with preference for suggested goals
			if (! justifiableGoals.isEmpty()) {
				return Chooser.randomChoice(justifiableGoals);
			}
			
			logger.info("Cannot select a goal to justify. I cry.");
			return null;

		} else {
			// Keep current.
			return activeJustifiableGoal;
		}
	}
	
	/**
	 * Drops given justifiable goal from the director process
	 * @param goal the goal to drop
	 * @param successfull whether goal was successfully justified
	 */
	private void dropJustifiableGoal(JustifiableGoalSchema goal, boolean successfull) {
		logger.info("Dropping justifiable goal " + goal.getGoalSchema().getType() + ", successful: " + successfull);
		justifiableGoals.remove(goal);

		// If it is the active justifiable goal too, forget it as the active justifiable goal.
		if (goal == activeJustifiableGoal) {
			activeJustifiableGoal = null;
		}

		if (successfull) {
			successfulJustificationRound = StoryTime.getTime();
		}
	}	
	
	/**
	 * Determines whether actor is ready to start justifying a new justifiable goal
	 * TODO: semantically vague like this.
	 * @return
	 */
	private boolean readyToJustify() {
		boolean rdy = getGoals().isEmpty() && notDoingAnything > 2 && successfulJustificationRound < StoryTime.getTime() - 1;
		if (rdy) {
			logger.fine("Ready to justify:\nnotDoingAnything = " + notDoingAnything + "\nsucc. justification round: " + successfulJustificationRound);
		} else {
			logger.fine("Not ready to justify:\nnotDoingAnything = " + notDoingAnything + "\nsucc. justification round: " + successfulJustificationRound);
		}
		return rdy;
	}

	/**
	 * Does agent-as-director stuff, such as executing the framing operators & events of the current plan, and adopting new
	 * justifiable goals.
	 * 
	 * For lack of a better name. Sigh.
	 */
	private void direct() {
		
		logger.info("Directing");
		
		// Execute framing operators & events of current plan (in deliberative layer)
		if (getPlanner() != null) {
			notDoingAnything = 0;
			framePlan(getPlanner());
		}
		
		notDoingAnything++;
		
		// If there are no more goals in-character, adopt justifiable goals.
		if (readyToJustify()) {
			logger.info("Ready to justify new goals.");
			
			// Adopt new goals that can be justified; only do this when there are none in the list and we're ready to justify. 
			// this is because it is quite a computationally intensive process to try to make a plan for the preconditions
			// of each goal in the DB.
			if (justifiableGoals.isEmpty()) {
				adoptJustifiableGoals();
			}
			
			// Select which justifiable goal should become active (only if we haven't finished justifying a goal this round already)
			activeJustifiableGoal = selectActiveJustifiableGoal();
						
		} else {
			logger.info("Not ready to justify.");
			return;
		}
		
		// Justify the goal that we aim for 
		if (activeJustifiableGoal != null) {			
			boolean successfull = false;
			if (activeJustifiableGoal.getJustifyingPlanner().plan() 
			&& (! activeJustifiableGoal.getJustifyingPlanner().planFinished())) {
					logger.info("Working on justifiable goal: " + activeJustifiableGoal.getGoalSchema().getType());				
					framePlan(activeJustifiableGoal.getJustifyingPlanner());
			} else {
				// Remember that we've already justified a goal this round.
				successfull = activeJustifiableGoal.getJustifyingPlanner().planFinished();
				dropJustifiableGoal(activeJustifiableGoal, successfull);
			}
		}
	}
	
	/**
	 * Of a given planner, see which events can be executed and execute them.
	 * @param planner a planner to execute events of
	 */
	private void executeEvents() {
		logger.fine("Trying to execute events.");

		// execute the events that are ready for execution
		for (String event: eventBuffer) {
			
			StoryEvent e = FabulaFactory.createEvent(event, m_characterAgent.getCharacterURI());
						
			// Ask Plot Agent to perform it!
			if (! ExecutionState.getInstance().performingOperator(e)) {
				m_characterAgent.writeGui("Executing event: " + e.getType());
				logger.info("Executing event: " + e.getType());
				ExecutionState.getInstance().registerPerformingOperator(e);
				m_characterAgent.getAgent().addBehaviour(new InitiateRequestPlotPerformOperatorBehaviour(m_characterAgent.getAgent(), e));
			} 
						
		}
		// Remove all events, executed or not.
		eventBuffer.clear();

	}
	
	public StoryAction selectAction() {
		StoryAction act = super.selectAction();		

		// See if we can execute events that are present in the plan (if we have no actions to pursue)
		// Selecting an action AND executing an event might be possible BUT this often looks really incongruent,
		// especially when the event pertains to this character
		if (act == null) {
			executeEvents();
		}
		
		return act;
	}
	
	
	
}
