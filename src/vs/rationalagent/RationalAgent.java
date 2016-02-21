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
package vs.rationalagent;

import jade.content.lang.Codec;
import jade.content.lang.sl.SLCodec;
import jade.content.onto.Ontology;
import jade.core.AID;
import jade.core.Agent;
import jade.domain.DFService;
import jade.domain.FIPAException;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.gui.GuiAgent;
import jade.gui.GuiEvent;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Logger;

import org.jpl7.Query;
import vs.Config;
import vs.StoryDomain;
import vs.characteragent.BasicCharacterAgent;
import vs.communication.VSTOntology;
import vs.debug.LogFactory;
import vs.knowledge.PrologKB;
import vs.rationalagent.behaviour.TraceBehaviour;
import vs.rationalagent.ui.RationalAgentGui;
import vs.rationalagent.ui.StoryAgentEvent;

/**
 * A JADE-Agent with OWL reasoning capabilities.
 *
 * @author Robert de Groote, Ivo Swartjes
**/

public abstract class RationalAgent extends GuiAgent implements IRationalAgent
{
	/////////////////////////////////////////////
	// ATTRIBUTES
	
	/////////////////////////////////////////////

	// Conversation ID
	protected static int cidCnt = 0;
		
	public static String CHARACTER_SERVICE = "character";
	
	public static String PLOT_SERVICE = "plot";
	public static String WORLD_SERVICE = "world";
	/** Descriptions of services for agents to provide **/
	public static String TRACER_SERVICE = "tracer";
	
	public static final String PROLOG_FILE = "loadDomain.pl";
	
	//Events:
	public static final	int					EXIT 				= 1000;
	public static final	int					LOAD_KB 			= 1100;
	
	public static final int					SAVE_KB				= 1101;
	public static final int					CLEAR_KB			= 1102;
	public static final int					CONSULT				= 1103;
	

	public static final int					QUERY_KB			= 1104;
	public static final int					TEST_KB				= 1105;
	//The GUI of this agent
	transient	protected	RationalAgentGui	myGui;

	// The Knowledge Manager for this agent
	private PrologKB m_worldKnowledge;
	
	// Agent Communication
	private Codec							m_Codec;
	
	/////////////////////////////////////////////
	// CONSTANTS
	/////////////////////////////////////////////

	private Ontology						m_Ontology;
	protected StoryDomain					m_currDomain;
	protected Set<StoryAgentEventListener> m_eventListeners;
	String cidBase ;
	// Debug support
	private AID	m_TracerAID = null;
	private AID	m_SnifferAID = null;
	private Logger logger;

	/////////////////////////////////////////////
	// CONSTRUCTORS AND INSTANTIATION
	/////////////////////////////////////////////
	
	public void addEventListener(StoryAgentEventListener listener) {
		m_eventListeners.add(listener);
	}
	
	/**
	 * creates a GUI for this agent
	 */
	protected void createGui()
	{
		myGui = new RationalAgentGui(this);
	}

	/* See IRational */
	public AID[] findServiceType( String type )
	{
		DFAgentDescription template = new DFAgentDescription();
		ServiceDescription sd = new ServiceDescription();
		sd.setType( type );
		template.addServices( sd );
		try {
			DFAgentDescription[] result = DFService.search( this, template );
			AID[] agents = new AID[result.length];
			
			for ( int i = 0; i < result.length; i++ ) {
				agents[ i ] = result[ i ].getName();
			}
			
			return agents;
		}
		catch (FIPAException fe ) {
			fe.printStackTrace();
		}
		
		return null;
	}

	public void fireEvent(StoryAgentEvent e) {
		for (StoryAgentEventListener l: m_eventListeners) {
			l.onStoryAgentEvent(e);
		}
	}	
	
	/* See IRationalAgent */
	public String genCID() { 
		if (cidBase==null) {
			cidBase = getLocalName() + hashCode() +
			System.currentTimeMillis()%10000 + "_";
		}
		return  cidBase + (RationalAgent.cidCnt++); 
	}
	
	public Agent getAgent() {
		return this;
	}

	/* See IRationalAgent */
	public Codec getCodec()
	{
		return m_Codec;
	}	

	/* See IRationalAgent */
	public PrologKB getKnowledgeManager()
	{
		return m_worldKnowledge;
	}

	/////////////////////////////////////////////
	// SETUP AND TAKEDOWN
	/////////////////////////////////////////////

	/* See IRationalAgent */
	public Ontology getOntology()
	{
		return m_Ontology;
	}
	
	/**
	 * @deprecated
	 */
	@Deprecated
	public AID getSniffer() {
		return m_SnifferAID;
	}		
	
	public StoryDomain getStoryDomain() {
		return m_currDomain;
	}
	
	/////////////////////////////////////////////
	// METHODS
	/////////////////////////////////////////////
	
	/* See IRationalAgent */
	/**
	 * @deprecated
	 */
	@Deprecated
	public AID getTracer() {
		return m_TracerAID;
	}
	
	
	/**
	 * The RationalAgentGui sends GuiEvents, which are handled by onGuiEvent
	 */
	@Override
	protected void onGuiEvent(GuiEvent ev)
	{
		String str;
		//ClauseIterator cit;
		//CNFSentence sent;
		
		switch(ev.getType())
		{
			//Exit Agent
			case EXIT:
				//takeDown();
				this.doDelete();
				break;
			case LOAD_KB:
				str = (String)ev.getParameter(0);
				myGui.writeConsole( "Loading KB " + str + "...", false );
				
				if (m_worldKnowledge.loadKB(str)) {
					myGui.writeConsole( "ok" );
				} else {
					myGui.writeConsole( "failed!" );
				}
				break;
			case CONSULT:
				str = (String)ev.getParameter(0);
				str = str.replaceAll("\\\\", "/");
				//str = "initSWCinderella.pl";
				myGui.writeConsole( "Consulting Prolog file " + str + " ...", false );
				if (m_worldKnowledge.consult(str)) {
					myGui.writeConsole( "ok" );
				} else {
					myGui.writeConsole( "failed!" );
				}
				break;
			case TEST_KB:
				myGui.writeConsole( "Testing KB ...", false );
				String test = new String("query(X, Y, Z).");
				myGui.writeConsole( "asking: " + test);
				Query q = new Query(test);
				if (q.hasSolution()) {
					myGui.writeConsole( "ok, answer was: " + q.oneSolution() );
				} else {
					myGui.writeConsole( "there was no answer." );
				}
				break;
			case SAVE_KB:
				str = (String) ev.getParameter( 0 );
				myGui.writeConsole( "Saving KB " + str + "...", false );
				if (m_worldKnowledge.saveKB(str)) {
					myGui.writeConsole( "ok." );
				} else {
					myGui.writeConsole( "failed!" );
				}
				break;
		}
	}
	
	protected StoryDomain readStoryDomain() {
		try {
			Properties p = new Properties();
			FileInputStream fis = new FileInputStream(Config.PROPERTIES_STATEFILE);
			p.load(fis);
			fis.close();
			
			Properties p2 = new Properties();
			FileInputStream fis2 = new FileInputStream(Config.PROPERTIESFILE);
			p2.load(fis2);
			fis2.close();
		    
		    // Read in story domain
			String currDomain = p.getProperty("current_story_domain");
    		String name = p2.getProperty("story_domain." + currDomain + ".name");
    		String author = p2.getProperty("story_domain." + name + ".author");
    		String desc = p2.getProperty("story_domain." + name + ".desc");
    		String ont = p2.getProperty("story_domain." + name + ".ontology");
    		String altEntry = p2.getProperty("story_domain." + name + ".ontology.local");
    		StoryDomain sd = new StoryDomain(name, author, desc, ont, altEntry);

			return sd;		    
		    
		} catch (IOException e) {
			logger.severe("Cannot read properties file " + Config.PROPERTIES_STATEFILE);
			e.printStackTrace();
		}
		
		return null;

	}

	public void removeEventListener(StoryAgentEventListener listener) {
		m_eventListeners.remove(listener);

	}
	
	
	/* See IRationalAgent */
	/**
	 * @deprecated
	 */
	@Deprecated
	public void setTracer(AID tracer) {
		m_TracerAID = tracer;
	}
	
	/**
	 * Initializes the agent. Registers codec and ontology used by the agent,
	 * Tries to hook up with the (@link TracerAgent), and sets up a reasoning
	 * context for reasoning with OWL.
	 * overrides jade.core.Agent.setup
	 */	
	@Override
	public void setup()
	{
		// Register this agent to the LogFactory, and make general
		// log file that will register everything logged within this
		// agent.
		
		LogFactory.registerAgent(this.getLocalName());
		logger = LogFactory.getLogger(this);
		
		m_eventListeners = new HashSet<StoryAgentEventListener>();
		
		// Register Codec
		// SL codec chosen because it is human-readable in contrast to LEAP; serialized Java objects
		// (see vs.communication package) are encoded in string in stead of binary format. Easier
		// for debugging purposes. (LEAP is useful for lightweight applications like mobile phone
		// applications, because LEAP is lighter.)
		
		// However, SL codec has a problem decoding AgentActions that have no slot, like the GiveControl
		// en JoinStory actions. Therefore changed back to LEAP codec for now.
		//
		// For more info about this choice see JADE methodology doc at 
		// 		http://jade.tilab.com/doc/JADE_methodology_website_version.pdf
		//
		// SOLUTION to this problem: SL codec requires an action to be wrapped in an Action object
		// REQUEST (Action (GiveControl) )
		
		//m_Codec = new LEAPCodec();
		m_Codec = new SLCodec();
   	 	getContentManager().registerLanguage( m_Codec );
   	 	
   	 	// Register Inter-Agent Ontology
   	 	m_Ontology = VSTOntology.getInstance();
    	getContentManager().registerOntology( m_Ontology );
    		   	
		// Create Prolog knowledgemanager
    	m_worldKnowledge = PrologKB.getInstance();
    	//m_worldKnowledge.setUp();
    	
  	
		//Create the Gui (delegated to all subclasses)
		createGui();
		
		m_currDomain = readStoryDomain();
		String domainPath = m_currDomain.getName() + "/";
		
		myGui.writeConsole("Loading story domain: "
				+ Config.DOMAINSPATH + domainPath);
		if (getKnowledgeManager().consult(
				Config.DOMAINSPATH + domainPath + RationalAgent.PROLOG_FILE)) {
			myGui.writeConsole("Succesfully consulted: "
					+ RationalAgent.PROLOG_FILE);
		} else {
			myGui.writeConsole("Could not consult: "
					+ RationalAgent.PROLOG_FILE);
		}
		
		

		// inform
		myGui.writeConsole( "Rational agent set up." );
		//trace(TracerAgent.INFO, "Agent " + getAID().getLocalName() + " ready.");
		logger.info("Rational agent " + getAID().getLocalName() + " ready.");
		
	}
	
	/////////////////////////////////////////////
	// EVENT-HANDLING
	/////////////////////////////////////////////
	
	/**
	 * overrides jade.core.Agent.takeDown
	 */	
	@Override
	public void takeDown()
	{
		//Close the gui:
		if(myGui != null)
		{
			myGui.setVisible(false);
			myGui.dispose();
		}
		
		// Deregister service from yellow pages
		try {
			DFService.deregister( this );
		}
		catch ( FIPAException fe ) {
			fe.printStackTrace();
		}
		
		super.takeDown();	
		
		System.exit(0);
	}
	
	/* See IRational */
	/**
	 * @deprecated
	 */
	@Deprecated
	public void trace( int verbosity, int depth, String message ) {
		addBehaviour(new TraceBehaviour(this, verbosity, depth, message));
	}
	/**
	 * Sends a message to the TracerAgent if the tracer-service has been found in
	 * the yellow pages.
	 * @deprecated
	 */
	@Deprecated
	public void trace( int verbosity, String message )
	{
		trace( verbosity, 0, message );
	}

	public void writeConsole(String message) {
		myGui.writeConsole(message);
	}
	
	public void writeGui(String msg) {
		myGui.writeConsole(msg);
	}
}
