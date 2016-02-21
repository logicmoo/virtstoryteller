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
import jade.domain.FIPANames;
import jade.gui.GuiEvent;
import jade.lang.acl.MessageTemplate;
import jade.proto.AchieveREResponder;
import jade.proto.ProposeResponder;
import jade.util.leap.Iterator;

import java.io.File;
import java.util.HashSet;
import java.util.Set;
import java.util.Vector;
import java.util.logging.Logger;

import javax.swing.JFileChooser;

import vs.Config;
import vs.characteragent.behaviour.ExplainerBehaviour;
import vs.characteragent.behaviour.ProposeResponderBehaviour;
import vs.characteragent.behaviour.ReceiveInformBehaviour;
import vs.characteragent.behaviour.RequestResponderBehaviour;
import vs.characteragent.ui.CharacterAgentGui;
import vs.communication.CharacterInfo;
import vs.communication.FabulaCausality;
import vs.communication.FabulaElement;
import vs.communication.IncomingSetting;
import vs.communication.NextRound;
import vs.communication.OperatorResult;
import vs.communication.StoryAction;
import vs.communication.StoryBelief;
import vs.communication.StoryPerception;
import vs.debug.LogFactory;
import vs.fabula.io.LanguageFilter;
import vs.knowledge.PrologKB;
import vs.knowledge.vocab.Fabula;
import vs.poplanner.PoPlanner;
import vs.rationalagent.RationalAgent;
import vs.rationalagent.StoryTime;
import vs.rationalagent.behaviour.RegisterBehaviour;

import com.hp.hpl.jena.shared.PrefixMapping;

/**
 * Character Agent
 * 
 * @author kruizingaEE
 * 
 * NOTE about explainability of agent (swartjes)
 *   - The structure to be implemented is that every fact, every goal, action, etc, gets an Individual name, so we can talk
 *     about it.
 *   - We are already "talking" about goals, actions etc using the vs.communication objects.
 *   - To use efficient cooperation, agents should not constantly have to encode and decode these objects, but USE them in 
 *     their agent structure.
 *   - Therefore, agents should create StoryGoal, StoryBelief etc as soon as possible, and then use them. E.g., current intention
 *     is (or contains) a StoryGoal. When making beliefs, remember them. Make an efficient way to query whether new facts are stored
 *     as agent beliefs. (does this become a double administration of knowledge?)
 */

public class BasicCharacterAgent extends RationalAgent implements
		ICharacterAgent {

	private static final long serialVersionUID = 5619493617571786259L;

	// Events:
	public static final int SET_CHARACTER_URI = 2000;
	public static final int SHOWAGENTID = 2001;
	public static final int SHOWACTIONS = 2002;
	public static final int DOACTION = 2003;
	public static final int SHOWPLANS = 2004;
	public static final int SETINTENTIONS = 2005;
	public static final int CREATEPLAN = 2007;
	public static final int SAVEEPISODICMEMORY = 2008;
	
	public static final String PROLOG_FILE = "load_character.pl";

	private Logger logger;
	private AID m_plotAgent = null;
	private String m_characterURI = null;

	protected ActorProcess m_characterProcess;
		
	protected IInterpretationModule m_interpretationModule;
	
	protected EpisodicMemory m_episodicMemory;
	
	protected ExplainerBehaviour m_explainerBehaviour;
	private Set<FabulaElement> m_elements;
	private Set<FabulaCausality> m_causalities;
	
	/**
	 * overwrites createGui() of superclass
	 */
	@Override
	protected void createGui() {
		myGui = new CharacterAgentGui(this);
	}

	/* See ICharacterAgent */
	public String getCharacterURI() {
		return m_characterURI;
	}

	/* See ICharacterAgent */
	public CharacterProcess getCharacterProcess() {
		return m_characterProcess;
	}

	public EpisodicMemory getEpisodicMemory() {
		return m_episodicMemory;
	}

	public IInterpretationModule getInterpretationModule() {
		return m_interpretationModule;
	}
	
	/* See ICharacterAgent */
	public AID getPlotAgent() {
		return m_plotAgent;
	}	

	/**
	 * Return the time (in rounds)
	 */
	public int getTime() {
		return StoryTime.getTime();
	}


	public void handleCharacterInfo(CharacterInfo c) {
		if (getCharacterURI() == null) {
			setCharacterURI(c.getIndividual());
		} else {
			logger.warning("Character info received, but agent already committed to character ID " + getCharacterURI());
		}
	}
		
		

	/**
	 * Handle an incoming setting by adding it to the KB.
	 * 
	 */
	public void handleIncomingSetting(IncomingSetting is) {
		
		Set<StoryBelief> beliefs = getInterpretationModule().adopt(is.getSetting());
		
		/* Do we go on and interpret these beliefs? And form affective dispositions? It would seem
		 * strange since we pretend this setting element "has always been the case", but if we introduce
		 * a corpse on the deck, it would be strange if characters don't have any disposition towards it.
		 * 
		 * It seems like we need to either:
		 * 	-- put such dispositions in the effect of the improv operator
		 *  -- simulate a "fake" appraisal for a few steps to see what the affective disposition becomes. 
		 *  -- not put affective things in improv operators, only "normal" things, and let the affective
		 *     response occur only upon perceptions that are an effect of settings introduced by the Plot Agent.
		 *     
		 * For now, we don't do anything with affect; just be neutral.
		 */
		

		
		/* Incoming settings might enable new improvisations (that required improvisations of which the setting
		 * is now a result); go through the improvisation behaviour again.
		 * 
		 * TODO: this can be awkward; if there are more setting updates, i.e. two effect triples of one improvisation action,
		 * the planner will make a new plan for each of them. This could potentially mean that there is temporarily no plan to make.
		 * Example: 
		 * (1) you can create a crate if there is no crate yet, and the effect is that there is now a crate on the ship.
		 * (2) you can create a bottle in a crate when there is a crate somewhere.
		 * After performing (1) and getting the setting update that there is now a crate (followed by: the crate is on the ship), 
		 * it invalidates (1), making it impossible to get the crate on the ship (although a subsequent setting change from the 
		 * execution of (1) will turn out to resolve that)
		 * 
		 * This problem is removed when we use the continuous planner. For now, make sure that the effects are ordered such that they don't
		 * invalidate preconditions.
		 */
		logger.info("Received new setting; updating plan.\nSetting: " + is.getSetting().getContentTriple());

		// No appraisal, only coping
		/*myGui.writeConsole("Re-adjusting plan after new setting information.");
		getCharacterProcess().getDeliberativeLayer().cope();*/
		
		// Shifted responsibility of skipping appraisal to whether or not a framing operator was accepted
		// by this character. Avoiding appraisal once is not going to do the trick; the agent will just appraise
		// the setting in the next cycle.
		getCharacterProcess().appraise();
		getCharacterProcess().cope();
		
	}

	/**
	 * Next round has no consequences yet.
	 */
	public void handleNextRound(NextRound n) {
		myGui.writeConsole("--- Next round starting ---");
		StoryTime.setTime(n.getRoundNumber());
		
		// The explainer behaviour tracks state (e.g., "what did I already send to the plot agent?")
		// hence, restart, in stead of adding a new one every time.
		if (m_explainerBehaviour != null) {
			m_explainerBehaviour.reset();
			addBehaviour(m_explainerBehaviour);
		} else {
			m_explainerBehaviour = new ExplainerBehaviour(this);
			addBehaviour(m_explainerBehaviour);
		}

	}

	/**
	 * Handle an incoming result of an operator (finished, aborted, etc)
	 */
	public void handleOperatorResult(OperatorResult or) {
			
		ExecutionState.getInstance().registerOperatorResult(or);
	}
	

	/**
	 * Handle a perception by adding it to the KB if it is positive and deleting
	 * it if it is negative. It expects a single fact.
	 */
	public void handlePerception(StoryPerception p) {
		// Make interpretations for the agent
		getInterpretationModule().interpret(p);
			
	}
	
	/**
	 * Handles the selection of an action to perform.
	 */
	public StoryAction handleSelectAction() {
		// This is the place for appraisal and coping: we have received all perceptions and setting changes.
		// TODO: there are race conditions between appraisal/coping and processing the fabula that the agent has
		//		produced. So appraisal/coping might or might not be using information about newly adopted goals etc.
		//		It is desired to make sure this information is _always_ used. But not sure how.
		myGui.writeConsole("Reactive and deliberative appraisal.");
		getCharacterProcess().appraise();
		myGui.writeConsole("Reactive and deliberative coping.");
		getCharacterProcess().cope();
		
		StoryAction selectedAction = null;
		
		// Already performing an action
		if (ExecutionState.getInstance().currentAction() != null) {
			// I am already doing an action.
			// TODO: this is a speedup shortcut; it would be nicer to do selectAction always and just see if the same action was selected.
			//		 this would take into account a changed environment instead of blindly pursuing the action
			logger.info("Not choosing a new action: already performing an action");
			myGui.writeConsole("Select action: already performing an action (namely " + ExecutionState.getInstance().currentAction().getIndividual() + ")");
			return null;
		} 
		
		selectedAction = getCharacterProcess().selectAction();
		
		// No action selected
		if (selectedAction == null) {
			// I don't want to do an action because I wouldn't know what to do.
			logger.info("Not choosing a new action: I have no idea what to do.");
			myGui.writeConsole("Select action: no idea what to do.");

		} else {

			// Tell execution state that this is the operator we're performing.
			ExecutionState.getInstance().registerPerformingOperator(selectedAction);
			
			// Handle enablements of action
			Set<String> enablements = PrologKB.getInstance().getEnablingFabulaElements(selectedAction);
			
			for (String enabler: enablements) {
				FabulaCausality enables = new FabulaCausality();
				enables.setSubjectIndividual(enabler);
				enables.setObjectIndividual(selectedAction.getIndividual());
				enables.setCausalProperty(Fabula.enables);
				m_causalities.add(enables);
			}
		
			myGui.writeConsole("Select action: " + selectedAction.getIndividual() + " (" + selectedAction.getType() + ")");
			logger.info("Select action: " + selectedAction.getIndividual() + " (" + selectedAction.getType() + ")");
		}
		
		return selectedAction;
	}
		
	/**
	 * The ActorAgentGui sends GuiEvents, which are handled by onGuiEvent
	 */
	@Override
	protected void onGuiEvent(GuiEvent ev) {
		// let superclass handle event first
		super.onGuiEvent(ev);
		
		Vector<String> simpleActionList;

		switch (ev.getType()) {

		case SAVEEPISODICMEMORY:
			File targetFile = null;
			String fileType = null;
			for (Iterator it = ev.getAllParameter(); it.hasNext(); ) {
				Object o = it.next();
				if (o instanceof File) {
					// Target file
					targetFile = (File) o;
				}
				if (o instanceof LanguageFilter) {
					fileType = ((LanguageFilter)o ).getLanguage();
				}
				if (targetFile != null && fileType != null) {
					getEpisodicMemory().saveFabula(
							targetFile, fileType);
				}

			}

		case CREATEPLAN:
			if (getCharacterProcess().getDeliberativeLayer().getPlanner() != null) {
				if (getCharacterProcess().getDeliberativeLayer().getPlanner().plan()) {
					myGui.writeConsole("Plan created succesfully.");
				} else {
					myGui.writeConsole("No plan was created.");
				}
				showList(getCharacterProcess().getDeliberativeLayer().getPlanner().getExecutableOperators(),
				"first actions in the plan");
			}
		}
	}
	
	/* See ICharacterAgent */
	public boolean setCharacterURI(String characterURI) {
		
		PrefixMapping pm = PrefixMapping.Factory.create();
		pm.setNsPrefixes(Config.namespaceMap);

		myGui.writeConsole("Assuming the character role of " + pm.shortForm(characterURI));
		myGui.setTitle(getAgent().getLocalName() + " playing " + pm.shortForm(characterURI));
		m_characterURI = characterURI;

		// swartjes: what does Prolog.setAgentID do? I cannot find its functionality. 
		if (PrologKB.getInstance().setAgentID(characterURI)) {

			m_characterProcess = new ActorProcess(this);
			return true;
		} else {
			return false;
		}

	}
	
	/* See ICharacterAgent */
	public void setPlotAgent(AID agent) {
		myGui.writeConsole("Setting PlotAgentAID from " + m_plotAgent + " to "
				+ agent);
		m_plotAgent = agent;

	}
	
	/*
	 * See IExplainable
	 */
	public Set<FabulaCausality> explainCausalities() {
		/*
		 * Explaining the agent means explaining the interpretation module, and the agent processes
		 */
		
		m_causalities.addAll(getInterpretationModule().explainCausalities());
		m_causalities.addAll(getCharacterProcess().explainCausalities());
		
		return m_causalities;
	}
	
	/*
	 * See IExplainable
	 */
	public Set<FabulaElement> explainElements() {
		
		/*
		 * Explaining the agent means explaining the interpretation module, and the agent processes
		 */

		m_elements.addAll(getInterpretationModule().explainElements());
		m_elements.addAll(getCharacterProcess().explainElements());
		
		return m_elements;
	}	

	// Call the PoPlanner to create a plan.


	// ///////////////////////////////////////////
	// EVENT-HANDLING
	// ///////////////////////////////////////////

	/**
	 * override jade.core.Agent.setup
	 */
	@Override
	public void setup() {
		super.setup();

		// Initialize logger
		logger = LogFactory.getLogger(this);
		logger.info("Setting up a Character Agent ...");

		m_episodicMemory = new EpisodicMemory(this);
		m_interpretationModule = new BasicInterpretationModule(this);
			
		m_elements = new HashSet<FabulaElement>();
		m_causalities = new HashSet<FabulaCausality>();

		myGui.writeConsole("Loading prolog environment: "
				+ Config.PROLOGFILESPATH + BasicCharacterAgent.PROLOG_FILE);
		if (getKnowledgeManager().consult(
				Config.PROLOGFILESPATH + BasicCharacterAgent.PROLOG_FILE)) {
			myGui.writeConsole("Succesfully consulted: "
					+ BasicCharacterAgent.PROLOG_FILE);
		} else {
			myGui.writeConsole("Could not consult: "
					+ BasicCharacterAgent.PROLOG_FILE);
		}
		
		// receive INFORM messages
		addBehaviour(new ReceiveInformBehaviour(this));

		// receive REQUEST messages
		MessageTemplate mt = AchieveREResponder
				.createMessageTemplate(FIPANames.InteractionProtocol.FIPA_REQUEST);
		addBehaviour(new RequestResponderBehaviour(this, mt));

		// receive PROPOSE messages		
		MessageTemplate mt2 = ProposeResponder
		.createMessageTemplate(FIPANames.InteractionProtocol.FIPA_PROPOSE);
		addBehaviour(new ProposeResponderBehaviour(this, mt2));

		// register this agent with DF
		addBehaviour(new RegisterBehaviour(this,
				RationalAgent.CHARACTER_SERVICE));

		// Show the GUI
		logger.info("Making Gui visible...");
		myGui.setVisible(true);
		logger.info("Succesfully set up a character agent.");
	}
	
	// Show a list in the gui
	private void showList(Vector<String> list, String typeName) {
		myGui.writeConsole("This agent has " + list.size() + " " + typeName,
				false);
		if (list.isEmpty()) {
			myGui.writeConsole(".");
		} else {
			myGui.writeConsole(":");
			for (int i = 0; i < list.size(); i++) {
				myGui.writeConsole(list.elementAt(i));
			}
		}
		;
	}
}