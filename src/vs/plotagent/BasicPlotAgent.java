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
import jade.core.behaviours.ParallelBehaviour;
import jade.core.behaviours.SequentialBehaviour;
import jade.domain.FIPANames;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.gui.GuiEvent;
import jade.lang.acl.MessageTemplate;
import jade.proto.AchieveREResponder;

import java.util.Iterator;
import java.util.Set;
import java.util.logging.Logger;

import vs.Config;
import vs.IExplainable;
import vs.communication.FabulaCausality;
import vs.communication.FabulaElement;
import vs.communication.NextRound;
import vs.communication.StoryAction;
import vs.debug.LogFactory;
import vs.fabula.IFabulaBuilder;
import vs.plotagent.behaviour.ActuateInspirationBehaviour;
import vs.plotagent.behaviour.ExplainerBehaviour;
import vs.plotagent.behaviour.InitiateRequestGiveControlBehaviour;
import vs.plotagent.behaviour.InitiateRequestPerformOperatorBehaviour;
import vs.plotagent.behaviour.InitiateRequestSelectActionBehaviour;
import vs.plotagent.behaviour.InitiateSubscriptionBehaviour;
import vs.plotagent.behaviour.ManageEpisodesBehaviour;
import vs.plotagent.behaviour.ManagePlotGoalBehaviour;
import vs.plotagent.behaviour.ReceiveInformBehaviour;
import vs.plotagent.behaviour.RequestResponderBehaviour;
import vs.plotagent.ui.AgentsChangedEvent;
import vs.plotagent.ui.PlotAgentGui;
import vs.plotagent.ui.StatusChangedEvent;
import vs.rationalagent.RationalAgent;
import vs.rationalagent.StoryTime;
import vs.rationalagent.behaviour.RegisterBehaviour;
import vs.rationalagent.behaviour.SendInformBehaviour;

/**
 * Plot Monitor agent
 * 
 * @author swartjes Created on 20-jul-2005
 */
public class BasicPlotAgent extends RationalAgent implements IPlotAgent, IExplainable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -7329902462752425416L;

	public static final int STATUS_WAITING = 1;
	public static final int STATUS_READY = 2;
	public static final int STATUS_STORY_OVER = 3;
	
	// Events:
	public static final int SHOWFABULA = 3000;
	public static final int GETSUGGESTIONS = 4000;

	//public static final String P_NARRATIVEINSPIRATION = Config.PLOTPROLOGFILESPATH + "narrativeInspirationModule.pl";
	public static final String PROLOG_FILE = "load_plot.pl";
	//public static final String P_AGENT = "BasicPlotAgent.pl";	


	// World agent
	private AID m_worldAgent = null;

	//
	private InitiateSubscriptionBehaviour subscribeToWorldAgentsBehaviour;
	private InitiateSubscriptionBehaviour subscribeToCharacterAgentsBehaviour;

	// The modules of this agent
	protected IPerceptionManager m_perceptionManager;
	protected IFabulaBuilder m_fabulaBuilder;
	protected ICharacterManager m_characterManager;
	protected IThreadManager m_threadManager;
	protected IInspirationModule m_inspirationModule;
	protected IPlotGoalManager m_plotGoalManager;
	
	protected ExplainerBehaviour m_explainerBehaviour;

	private int m_time;
	private int m_status;

	private Logger logger;

	/**
	 * Builds a network of relations with other agents using the Directory
	 * Facilitator (DF) to subscribe to services the Plot Agent is interested
	 * in. These are World Agents (WORLD service) and Character Agents
	 * (CHARACTER service).
	 * 
	 * NOTE: unfortunately, it is not possible to send ONE subscription message
	 * specifying we want to listen to world agents OR character agents. When we
	 * add two ServiceDescriptions to the ACL message, it is interpreted as a
	 * query for agents that provide both the world_service AND the
	 * character_service, and obviously that is not the intention.
	 * 
	 * Therefore, we need TWO subscription messages. However, for some reason
	 * these are part of the same protocol (more concretely, they have the same
	 * reply-with value. That means that incoming messages from the DF to inform
	 * us that there is a new agent with the requested service, is
	 * non-deterministically handled by either the first or the second
	 * InitiateSubscriptionBehaviour. That is the reason why checking the
	 * service type is done in a separate method (the handleSubscription()
	 * method of BasicPlotAgent), and not in the behaviours themselves.
	 * 
	 * SECOND NOTE: the Plot Agent doesn't subscribe to agents _leaving_ the
	 * system. JADE doesn't support this. therefore, extra attention should be
	 * paid to checking whether agents are still there, i.e., if a REQUEST is
	 * answered by a FAILURE, some cleanup should be done.
	 */
	public void buildAgentRelations() {

		// Listen to world agents
		myGui.writeConsole("Setting up agent subscriptions...", false);

		subscribeToWorldAgentsBehaviour = new InitiateSubscriptionBehaviour(
				this, RationalAgent.WORLD_SERVICE);
		addBehaviour(subscribeToWorldAgentsBehaviour);

		// Listen to character agents

		subscribeToCharacterAgentsBehaviour = new InitiateSubscriptionBehaviour(
				this, RationalAgent.CHARACTER_SERVICE);
		addBehaviour(subscribeToCharacterAgentsBehaviour);
		myGui.writeConsole("DONE.");

	}

	/**
	 * overwrites createGui() of superclass
	 */
	@Override
	protected void createGui() {
		myGui = new PlotAgentGui(this);
	}

	public Set<FabulaCausality> explainCausalities() {
		return m_perceptionManager.explainCausalities();
	}

	public Set<FabulaElement> explainElements() {
		return m_perceptionManager.explainElements();
	}

	/* See IPlotAgent */
	public ICharacterManager getCharacterManager() {
		return m_characterManager;
	}

	/* See IPlotAgent */
	public IFabulaBuilder getFabulaBuilder() {
		return m_fabulaBuilder;
	}

	/* See IPlotAgent */
	public IInspirationModule getInspirationModule() {
		return m_inspirationModule;
	}

	/* See IPlotAgent */
	public IPerceptionManager getPerceptionManager() {
		return m_perceptionManager;
	}

	/* See IPlotAgent */
	public IPlotGoalManager getPlotGoalManager() {
		return m_plotGoalManager;
	}

	/* See IPlotAgent */
	public IThreadManager getThreadManager() {
		return m_threadManager;
	}
	
	/* See IPlotAgent */
	public int getTime() {
		return StoryTime.getTime();
	}

	/**
	 * Getter for World Agent
	 * 
	 * @return AID of the World agent
	 */
	public AID getWorldAgent() {
		return m_worldAgent;
	}

	/**
	 * If the Plot Agent receives an Action from a character agent, this method
	 * handles this by sending a REQUEST to the World Agent to execute the
	 * Action.
	 * 
	 * @param act the Action sent by the character agent
	 */
	public void handleIncomingAction(StoryAction act) {
		if (m_worldAgent != null) {

			addBehaviour(new InitiateRequestPerformOperatorBehaviour(this, act));

		}
	}
	
	
	/**
	 * Handles taking aboard a new world agent (if the plot agent has not already committed to one)
	 * 
	 * @param agentID the AID (JADE agent ID) of the world agent 
	 */
	protected void handleNewWorldAgent(AID agentID) {

		if (m_worldAgent == null) {
			// This is the first world agent

			logger.info("Found world agent " + agentID
					+ ": REQUESTing it to give control.");

			addBehaviour(new InitiateRequestGiveControlBehaviour(this, agentID));

		}
	}

	/**
	 * Handles taking aboard a new character agent that has subscribed (but not agreed to join the story yet).
	 * 
	 * @param agentID the AID (JADE agent ID) of the character agent
	 */
	protected void handleSubscribedCharacterAgent(AID agentID) {
		m_characterManager.addCharacterAgent(agentID);
		// ((PlotAgentGui)myGui).refreshAgentList();
		fireEvent(new AgentsChangedEvent(this));
		// addSubscribedCharacterAgent(agentID);
	}

	/**
	 * Deals with incoming subscriptions or UNsubscriptions from both world agents and character agents. 
	 * Dispatches character agent subscriptions to the CharacterManager, and handles world agent subscriptions
	 * directly.
	 */
	public void handleSubscription(DFAgentDescription dfds) {

		if (! dfds.getAllServices().hasNext()) {
			
			// We have a DEREGISTER
			logger.info("Agent deregistered: " + dfds.getName());
			
			if (m_worldAgent.equals(dfds.getName()) ) {
				// The World Agent deregisters. What next? 
				// If the story has started already, the story world state is wrecked; stop this agent.
				if (m_time > 0) {
					logger.severe("World agent left; we are left without any world state.");
					myGui.writeConsole("--- END OF STORY GENERATION ---");
					m_status = STATUS_STORY_OVER;
					fireEvent(new StatusChangedEvent(this, m_status));
				}
				
				m_worldAgent = null;
				fireEvent(new AgentsChangedEvent(this));
			} else {
				// Check whether it is a character agent (not necessary, but makes the code more readable
				getCharacterManager().removeCharacterAgent(dfds.getName());
				fireEvent(new AgentsChangedEvent(this));
			}
		} else {
		
			// We have a REGISTER
			for (Iterator it = dfds.getAllServices(); it.hasNext();) {
				ServiceDescription sd = (ServiceDescription) it.next();
	
				// To find WORLD and CHARACTER agents
				if (sd.getType().equals(RationalAgent.WORLD_SERVICE)) {
					logger
							.info("Subscription received about agent with WORLD_SERVICE: "
									+ dfds.getName());
					handleNewWorldAgent(dfds.getName());
				} else if (sd.getType().equals(RationalAgent.CHARACTER_SERVICE)) {
					logger
							.info("Subscription received about agent with CHARACTER_SERVICE: "
									+ dfds.getName());
					handleSubscribedCharacterAgent(dfds.getName());
				} else {
					logger.warning("Subscription received for unknown service "
							+ sd.getName() + ". This shouldn't happen!");
				}
	
			}
		}
	}

	/* See IPlotAgent */
	public void nextRound() {
		
		if (m_worldAgent == null) {
			myGui.writeConsole("No World Agent available (yet).");
			return;
		}

		m_time++;
		StoryTime.tick();
		logger.info("--- New round is starting ---");
		myGui.writeConsole("--- New round starting (time = " + StoryTime.getTime() + ") ---");
		
		// The explainer behaviour tracks state (e.g., "what did I already log in the fabula?")
		// hence, restart, instead of adding a new one every time.
		if (m_explainerBehaviour != null) {
			m_explainerBehaviour.reset();
			addBehaviour(m_explainerBehaviour);
		} else {
			m_explainerBehaviour = new ExplainerBehaviour(this);
			addBehaviour(m_explainerBehaviour);
		}
		
		// Make array with all character agents as receivers
		AID[] receivers = new AID[m_characterManager.getCastedCharacters().size() + 1];
		receivers = m_characterManager.getCastedCharacters().toArray(receivers);
		receivers[receivers.length - 1] = m_worldAgent;

		// Create a behaviour for a "round" in the simulation, which is a behaviour that 
		// executes all its sub-behaviours in sequence.
		SequentialBehaviour roundBehaviour = new SequentialBehaviour() {
			
			private static final long serialVersionUID = 2905024314974090022L;

			@Override
			public int onEnd() {
				// pulse(); // to automate the rounds (CAREFUL, creates infinite loop due to absence of recognition of end of story)
				logger.info("Round behaviour ended!");
				m_status = STATUS_READY;
				fireEvent(new StatusChangedEvent(this, m_status));
				myGui.writeConsole("Ending round behaviour.");
				return super.onEnd();

			}

			@Override
			public void onStart() {
				m_status = STATUS_WAITING;
				fireEvent(new StatusChangedEvent(this, m_status));
				myGui.writeConsole("Starting round behaviour...");
			}
		};

		// Add sub-behaviours to the round behaviour:
		
		// Manage episodes (see if current has ended, start new ones, etc)
		roundBehaviour.addSubBehaviour(new ManageEpisodesBehaviour(this));
		
		// Manager plot goals
		roundBehaviour.addSubBehaviour(new ManagePlotGoalBehaviour(this));
		
		// Tell agents that a new round starts
		NextRound round = new NextRound();
		round.setRoundNumber(m_time);
		roundBehaviour.addSubBehaviour(new SendInformBehaviour(this, receivers, round));
		
		// Activate suggestions gotten from narrative inspiration
		roundBehaviour.addSubBehaviour(new ActuateInspirationBehaviour(this));
		
		// Ask character agents to select an action	
		ParallelBehaviour requestInTurnBehaviour = new ParallelBehaviour();
		// Send to all character agents
		// (note: this requires the behaviour to be an AchieveREInitiator and NOT
		//  a SimpleAchieveREInitiator!)
		for (AID receiver: getCharacterManager().getCastedCharacters()) {
			requestInTurnBehaviour.addSubBehaviour(new InitiateRequestSelectActionBehaviour(this, receiver));			
		}

		roundBehaviour.addSubBehaviour(requestInTurnBehaviour);
		
		// Add the round behaviour to the agent's behaviour pool.
		addBehaviour(roundBehaviour);

	}
	
	/**
	 * The ActorAgentGui sends GuiEvents, which are handled by onGuiEvent
	 */
	@Override
	protected void onGuiEvent(GuiEvent ev) {
		// let superclass handle event first
		super.onGuiEvent(ev);
		String str;

		switch (ev.getType()) {
		case SHOWFABULA:
			// m_graphWindow = new GraphGui(new
			// FabulaGraphFiller(getStoryBuilder().getKnowledgeManager()));
			// ((WorldAgentGui)this.myGui).drawGraph();
			break;

		case GETSUGGESTIONS:
			break;
		}
	}
	
	/**
	 * override jade.core.Agent.setup
	 */
	@Override
	public void setup() {

		super.setup();

		logger = LogFactory.getLogger(this);

		// Modules
		m_perceptionManager = new BetterPerceptionManager(this);
		m_fabulaBuilder = new BroadcastingFabulaBuilder(this);
		m_characterManager = new BasicCharacterManager(this);
		m_inspirationModule = new BasicInspirationModule(this);
		m_plotGoalManager = new BasicPlotGoalManager(this);
		m_threadManager = new BasicThreadManager(this);
		
		// display the GUI
		myGui.setVisible(true);
	

		// Load prolog side of agent
		myGui.writeConsole("Loading prolog environment...", false);
		if (getKnowledgeManager().consult(
				Config.PROLOGFILESPATH + BasicPlotAgent.PROLOG_FILE)) {
			myGui.writeConsole("DONE.");
		} else {
			myGui.writeConsole("FAILURE consulting " + Config.PROLOGFILESPATH
					+ BasicPlotAgent.PROLOG_FILE);
		}

		addBehaviour(new RegisterBehaviour(this, RationalAgent.PLOT_SERVICE)); //register this agent with DF
		addBehaviour(new ReceiveInformBehaviour(this));
		MessageTemplate mt = AchieveREResponder
			.createMessageTemplate(FIPANames.InteractionProtocol.FIPA_REQUEST);
		addBehaviour(new RequestResponderBehaviour(this, mt));

		// Set up relations with world agents and character agents
		buildAgentRelations();

		myGui.writeConsole("Plot agent ready.");
		m_status = STATUS_READY;

	}

	/**
	 * Setter for World Agent
	 */
	public void setWorldAgent(AID worldAgent) {
		logger.info("Committing to world agent " + worldAgent.getLocalName());
		myGui.writeConsole("Committing to world agent "
				+ worldAgent.getLocalName());
		m_worldAgent = worldAgent;

		// Don't cancel subscription; World Agent may deregister and we need to receive notification of that.
		//subscribeToWorldAgentsBehaviour.cancelSubscription();

		fireEvent(new AgentsChangedEvent(this));

		// ((PlotAgentGui)myGui).refreshAgentList();
	}

	// ///////////////////////////////////////////
	// EVENT-HANDLING
	// ///////////////////////////////////////////

	@Override
	public void takeDown() {

		// Cancel subscriptions
		// (potential problem if this is not done: new run of an Agent using the
		// same
		// name receives responses to orphan subscriptions).
		subscribeToWorldAgentsBehaviour.cancelSubscription();
		subscribeToCharacterAgentsBehaviour.cancelSubscription();

		// Take it down
		super.takeDown();
	}

}
