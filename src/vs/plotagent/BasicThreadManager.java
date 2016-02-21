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

import jade.core.AID;

import java.util.Vector;
import java.util.logging.Logger;

import vs.IAgent;
import vs.communication.GoalSchema;
import vs.communication.IncomingSetting;
import vs.communication.RDFtriple;
import vs.communication.StorySettingElement;
import vs.debug.LogFactory;
import vs.knowledge.PrologKB;
import vs.knowledge.vocab.Fabula;
import vs.plotagent.BetterPerceptionManager.OperatorEffects;
import vs.plotagent.behaviour.InitiateRequestUseSuggestionBehaviour;
import vs.rationalagent.behaviour.SendInformBehaviour;
import vs.utils.UniqueId;

public class BasicThreadManager implements IThreadManager {    // This name (and various others) gives me the association of Java Threads. Rename to BasicPlotThreadManager? -- Ivo
	
	protected class ThreadEnableResult implements IPlotGoalManager.IPlotGoalResult {
		private PlotThread thread;
		private BasicThreadManager manager;

	
		public ThreadEnableResult(PlotThread thread, BasicThreadManager manager) {
			this.thread = thread;
			this.manager = manager;
		}
		
		public void handleFailure () {			
		}
		
		public void handleSuccess () {
			manager.executeThread (thread);
		}
	}
	private IPlotAgent ownerAgent;
	//private String m_currentThread;
	
	PlotThread thread;
	
	protected Logger logger;
	
	protected StoryPhase phase;
	
	private boolean startingThread;
	
	public BasicThreadManager(IPlotAgent owner) {
		ownerAgent = owner;
		
		logger = LogFactory.getLogger(this);
		
		startingThread = false;
		
		thread = null;
		
		phase = StoryPhase.none;
		
		setPhase (StoryPhase.exposition);
	}
	
	/**
	 * Signals that a thread is finished (i.e. that a character has achieved his resolution goal
	 */
	public void finishThread (PlotThread thread) {
		thread.setState (PlotThread.FINISHED);
	}
	
	
	public void addThread(String threadString) {
		logger.info ("Adding thread: " + threadString);
		thread = new PlotThread (this, threadString);
		thread.setState (PlotThread.STARTING);
		startingThread = true;
		logger.info ("Trying to fulfill preconditions: " + thread.getPreconditions ());
		ownerAgent.getPlotGoalManager().addGoal (new IPlotGoalManager.PlotGoal(thread.getPreconditions (), new ThreadEnableResult (thread, this)));		
	}

	public void executeThread(PlotThread thread) {
		// TODO: Execution logic
		// ...
		logger.info("Executing thread: " + thread.getPrologName ());
				
		
		// Cast characters
		Vector<String> neededCharacters = thread.getCharacters ();
		
		logger.info("Needed characters for this thread: " + neededCharacters);
		
		// Take casting responsibility over from Prolog 
		for (String character: neededCharacters) {
			//PrologKB.getInstance().castedCharacter(character);
			String character2 = PrologKB.removeQuotes(character);
			ownerAgent.getCharacterManager().addWantedCharacter(character2);
		}

		// Cast characters
		Vector<String> settings = thread.getSettings ();

		// Handle new setting definitions by sending to WA and CA
		for (String setting: settings) {
			StorySettingElement sse = new StorySettingElement();
			sse.setIndividual(UniqueId.generateUniqueIndividual("Setting", "plotagent"));
			sse.setType(Fabula.SettingElement);
			sse.setCharacter("plotagent");
			
			// TODO: are all triples "positive"? or can you also have settings like "there was no beer."
			// TODO: this does not yet work.
			
			
			Vector<RDFtriple> tripleList = PrologKB.getInstance().conditionToTripleList(setting);
			for (RDFtriple triple: tripleList) {
				logger.info("Adding triple to setting: " + triple);
				sse.addContentTriple(triple);
			}
			
			ownerAgent.getCharacterManager().addSettingChange(tripleList);
			
			IncomingSetting is = new IncomingSetting();
			is.setSetting(sse);
			
			// Send to WA and all CA's
			// TODO: store such setting changes! New agents need to receive them too! And in the right order!
			// TODO: receiveInformBehaviour also contains similar code. Put in common place?
			for (AID character : ownerAgent.getCharacterManager()
					.getCastedCharacters()) {					
				
				AID[] receiver = new AID[1];
				receiver[0] = character;
				logger.info("Sending setting to character " + receiver[0]);
				ownerAgent.getAgent().addBehaviour(new SendInformBehaviour(ownerAgent.getAgent(), receiver,
						is));
			}
			
			AID wa = ownerAgent.getWorldAgent();
			AID[] receiver = new AID[1];
			receiver[0] = wa;
			logger.info("Sending setting to world agent: " + receiver[0]);
			ownerAgent.getAgent().addBehaviour(new SendInformBehaviour(ownerAgent.getAgent(), receiver,
					is));
			
			ownerAgent.getFabulaBuilder().addFabulaElement(sse);
		}

		thread.setState (PlotThread.STARTED);
		
		setPhase (StoryPhase.rising_action);
				
		// Use goal definitions from Prolog and give to characters
		// ... how? by a pulse, being called every round?
		// For now, character manager does this by querying thread manager
		
	}

	public void setPhase (StoryPhase phase) {
		
		String condition = "[condition(true, [fact('http://www.owl-ontologies.com/FabulaKnowledge.owl#story_phase', is, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#" + phase + "')])]";
		
		ownerAgent.getPlotGoalManager().addGoal (new IPlotGoalManager.PlotGoal(condition, null));
	
	}
	
	/**
	 *  Assigns a resolution goal to a character.
	 *  
	 *  @param goal the goal to assign to the character (the character is embedded in the goal)
	 *  @param thread the plot thread this assignment comes from; used for logging purposes
	 */
	public void assignResolutionGoal (Goal goal, PlotThread thread) {
		
		GoalSchema gs = new GoalSchema();
		gs.setCharacter(goal.getCharacter ());
		String type = PrologKB.getInstance().getSchemaType(goal.getPrologDescription ());
		gs.setIndividual(UniqueId.generateUniqueIndividual("ResolveGoal", PrologKB.fromNrSign(goal.getCharacter ())));
		gs.setType(type);
		gs.setPrologDescription(goal.getPrologDescription ());
		logger.info ("Looking for agent to character: " + goal.getCharacter ());
		
		AID agent = ownerAgent.getCharacterManager().getAgentForStoryWorldRepresentation (goal.getCharacter ());
		logger.info ("Agent query result, normal: " + (agent != null ? " not null" : "null"));
		ownerAgent.getAgent().addBehaviour(new InitiateRequestUseSuggestionBehaviour(ownerAgent.getAgent(), agent, gs));
		
		logger.info ("Assigning resolution goal " + goal + " from thread " + thread);
		
		// TODO: remove
		setPhase (StoryPhase.falling_action);
	}
	
	
	public IAgent getAgent() {
		return ownerAgent;
	}
	
	public PlotThread getCurrentThread() {
		return thread;
	}
	
	public Vector<String> getPossibleThreads() {
		
		Vector<String> possibleThreads = PrologKB.getInstance().possibleThreads();
		
		return possibleThreads;
	}
	
	// Get thread goals for given character
	public Vector<Goal> getThreadGoals(String characterURI) {
		Vector<Goal> threadGoals = thread.getGoals (characterURI);
		
		return threadGoals;
	}

	public boolean startingThread () {
		return startingThread;
	}
	
/*	public void pulse() {
		if (m_currentThread == null) {
			// Start new one
			Vector<String> possibleThreads = getPossibleThreads();
			logger.info("Possible threads at this point: " + possibleThreads);
			if (possibleThreads.iterator().hasNext()) {
				executeThread(possibleThreads.iterator().next());
			} else {
				logger.severe("No threads possible at this point! Make sure there are thread definitions in the domain and at least one that is always possible for the given domain.");
			}
		}
	}*/

}
