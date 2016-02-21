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
package vs.plotagent;

import jade.core.Agent;

import java.util.List;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import vs.IAgent;
import vs.characteragent.ExecutionState;
import vs.communication.FramingOperator;
import vs.communication.InferenceOperator;
import vs.communication.StoryAction;
import vs.communication.StoryEvent;
import vs.debug.LogFactory;
import vs.knowledge.PrologKB;
import vs.plotagent.behaviour.InitiateRequestPerformOperatorBehaviour;
import vs.plotagent.behaviour.InitiateRequestPlotPlotPerformOperatorBehaviour;
import vs.poplanner.PoPlanner;
import vs.utils.UniqueId;

/**
 * @author tommasse
 *
 */
public class BasicPlotGoalManager implements IPlotGoalManager {

	public class BasicPlotGoal extends PlotGoal {
	
		protected PoPlanner m_planner;

		BasicPlotGoal (PlotGoal goal) {
			super(goal);
			m_planner = new PoPlanner("_");
			m_planner.setGoals(	getConditions ());			
		}
		
		public PoPlanner getPlanner() {
			return m_planner;
		}

		public boolean perform(boolean fail) {
			
			if (m_planner.plan()) {			
				// We have a successful plan.
				
				logger.info("Plan: " + m_planner.getPlan());
				
				// Is it already done?
				if (m_planner.planFinished()) {
				
					logger.warning("Goal state is true.");
					
					success ();
					return true;
				}
				
				// Is it not done, but there are no actions to choose?
				String result = null;
				Vector<String> firstActions = m_planner.getExecutableOperators();
				if (firstActions == null) {
					logger.severe("firstActions == null: this should NEVER happen");
					//result = "null";
				} else if (firstActions.isEmpty()) {
					// Do nothing

					logger.setLevel(Level.FINE);
					
					logger.info("Framing plan: " + m_planner.getPlan());
					
					Vector<String> framingOperators = m_planner.getExecutableFramingOperators();
					logger.info("Got framing operators");
					Vector<String> inferenceOperators = m_planner.getExecutableInferenceOperators();
					logger.info("Got inference operators");
					Vector<String> events = m_planner.getExecutableEvents();
					logger.info("Got events");
					
					for (String inferenceOp: inferenceOperators) {
						InferenceOperator io = new InferenceOperator();
						logger.info("Creating inference operator");
						
						// To know who invented this operator
						io.setCharacter("plot"); 
						// Below info is not really needed but might be useful for debugging purposes.
						io.setType(PrologKB.getInstance().getSchemaType(inferenceOp));
						io.setIndividual(UniqueId.generateUniqueIndividual("Framing",
								PrologKB.fromNrSign("plot")));
						io.setPrologDescription(inferenceOp);
						
						// Ask Plot Agent to perform it!
						if (! ExecutionState.getInstance().performingOperator(io)) {
							logger.info("Executing inference operator: " + io.getType());
							ExecutionState.getInstance().registerPerformingOperator(io);
							((Agent)ownerAgent).addBehaviour(new InitiateRequestPlotPlotPerformOperatorBehaviour(((Agent)ownerAgent), io));
						}
					}
					
					for (String framingOp: framingOperators) {
						FramingOperator fo = new FramingOperator();
						logger.info("Creating framing operator");
						
						// To know who invented this operator
						fo.setCharacter("plot"); 
						// Below info is not really needed but might be useful for debugging purposes.
						fo.setType(PrologKB.getInstance().getSchemaType(framingOp));
						fo.setIndividual(UniqueId.generateUniqueIndividual("Framing",
								PrologKB.fromNrSign("plot")));
						fo.setPrologDescription(framingOp);
						
						// Ask Plot Agent to perform it!
						if (! ExecutionState.getInstance().performingOperator(fo)) {
							logger.info("Executing framing operator: " + fo.getType());
							ExecutionState.getInstance().registerPerformingOperator(fo);
							((Agent)ownerAgent).addBehaviour(new InitiateRequestPlotPlotPerformOperatorBehaviour(((Agent)ownerAgent), fo));
						}
					}
					
					for (String event: events) {
						StoryEvent e = new StoryEvent();
						
						// Below info is not really needed but might be useful for debugging purposes.
						e.setCharacter("plot"); 
						e.setType(PrologKB.getInstance().getSchemaType(event));
						e.setIndividual(UniqueId.generateUniqueIndividual("Event",
								PrologKB.fromNrSign("plot")));
						e.setPrologDescription(event);
						
						// Ask Plot Agent to perform it!
						if (! ExecutionState.getInstance().performingOperator(e)) {
							logger.info("Executing event: " + e.getType());
							ExecutionState.getInstance().registerPerformingOperator(e);
							((Agent)ownerAgent).addBehaviour(new InitiateRequestPlotPlotPerformOperatorBehaviour(((Agent)ownerAgent), e));
						} 
									
					}		
					
					
					
				} else {
					// Choose one of the first actions
					
					/* Not going to happen, since no actions can be scheduled due to the lack of characters.
					 * However, I'm keeping it here since it might be possible that the plot goal manager can
					 * steer NPC's or something in the future.
					 */
					
					
					result = firstActions.firstElement();
				
					StoryAction a = new StoryAction();
					a.setCharacter("plot");
					a.setPrologDescription(result);
					a.setIndividual(UniqueId.generateUniqueIndividual("Action",
							PrologKB.fromNrSign("plot")));
					a.setType(PrologKB.getInstance().getSchemaType(result));
					a.setAgens(PrologKB.getInstance().getSchemaAgens(result));
					a.setPatiens(PrologKB.getInstance().getSchemaPatiens(result));
					a.setTarget(PrologKB.getInstance().getSchemaTarget(result));
					a.setInstrument(PrologKB.getInstance().getSchemaInstrument(result));
					
					((Agent)ownerAgent).addBehaviour(new InitiateRequestPerformOperatorBehaviour((Agent) ownerAgent, a));

					
				/*	fabulaElements.add(a);
					
					// Create motivation causality
					FabulaCausality fc = new FabulaCausality();
					fc.setSubjectIndividual(activeGoal.getGoalSchema().getIndividual());
					fc.setObjectIndividual(a.getIndividual());
					fc.setCausalProperty(Fabula.motivates);
					
					fabulaCausalities.add(fc);*/
	
					
					return false;
				}
			} else {
				// TODO: no plan can be made; interpet as failed outcome?
				logger.info("No plan can be made");
				
				if (fail) {
					logger.info("Executing fail condition");
					failure ();
				}
				
				return fail;
			}
				
			return false;

		}
		
	}
	private IPlotAgent ownerAgent;
	protected Logger logger;

	protected List <BasicPlotGoal>plotGoals;
	protected Vector <BasicPlotGoal> newGoals;
	
	public BasicPlotGoalManager(IPlotAgent owner) {
		ownerAgent = owner;
		plotGoals = new java.util.LinkedList<BasicPlotGoal>();
		newGoals = new java.util.Vector<BasicPlotGoal>();
		logger = LogFactory.getLogger(this);
	}
	
	/* (non-Javadoc)
	 * @see vs.plotagent.IPlotGoalManager#addGoal(vs.plotagent.IPlotGoalManager.PlotGoal)
	 */
	@Override
	public void addGoal(PlotGoal goal) {
		// this is so that goals can add additional goals to the manager while in perform steps
		logger.info ("Adding goal " + goal + " to plot goal manager");
		newGoals.add (new BasicPlotGoal (goal));
	}

	
	public IAgent getAgent() {
		return ownerAgent;
	}

	/* (non-Javadoc)
	 * @see vs.plotagent.IPlotGoalManager#performSteps()
	 */
	@Override
	public void performSteps(boolean fail) {
		do {
			plotGoals.addAll (newGoals);
			newGoals.clear ();
			java.util.Iterator<BasicPlotGoal> it = plotGoals.iterator();
			while (it.hasNext ())
				if (it.next ().perform (fail))
					it.remove ();
		} while (!newGoals.isEmpty ());
	}
}
