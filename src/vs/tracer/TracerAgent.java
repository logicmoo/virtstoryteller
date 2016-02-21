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
package vs.tracer;

import jade.content.lang.Codec;
import jade.content.lang.leap.LEAPCodec;
import jade.content.onto.Ontology;
import jade.core.AID;
import jade.domain.DFService;
import jade.domain.FIPAException;
import jade.gui.GuiAgent;
import jade.gui.GuiEvent;
import vs.communication.VSTOntology;
import vs.rationalagent.RationalAgent;
import vs.rationalagent.behaviour.RegisterBehaviour;


public class TracerAgent extends GuiAgent
{
	/////////////////////////////////////////////
	// ATTRIBUTES
	/////////////////////////////////////////////

	/**
	 * 
	 */
	private static final long serialVersionUID = -67964627327157007L;

	/////////////////////////////////////////////
	// CONSTANTS
	/////////////////////////////////////////////
	public static final int	MSG_RECEIVE			= 1;
	
	public static final int	MSG_SEND			= TracerAgent.MSG_RECEIVE;
	public static final int FINE = 10;
	
	public static final int INFO = 5;
	public static final int SEVERE = 1;
	
	//The GUI of this agent
	transient	protected	TracerAgentGui	myGui;
	private int				m_traceDepth = 10;
	private int				m_verbosityLevel = 30;
	// Agent Communication
	private Codec							m_Codec;
	private Ontology						m_Ontology;

	/**
	 * create a GUI for this agent
	 */
	protected void createGui()
	{
		myGui = new TracerAgentGui( this );
	}

	public Codec getCodec()
	{
		return m_Codec;
	}
	
	/////////////////////////////////////////////
	// SETUP AND TAKEDOWN
	/////////////////////////////////////////////

	/////////////////////////////////////////////
	// CONSTRUCTORS AND INSTANTIATION
	/////////////////////////////////////////////
	public Ontology getOntology()
	{
		return m_Ontology;
	}
	
	public void handleTrace( AID sender, int verbosity, int depth, String message )
	{
		if ( verbosity > m_verbosityLevel ) {
			return;
		}
			
		if ( depth > m_traceDepth ) {
			return;
		}

		String prefix = sender.getLocalName();
		prefix = prefix + ":\t";
			
		for ( int i = 0; i < depth; i++ ) {
			message = "___" + message;
		}
			
		myGui.writeConsole( prefix + message );
	}		
	
	/////////////////////////////////////////////
	// METHODS
	/////////////////////////////////////////////

	@Override
	protected void onGuiEvent(GuiEvent ev)
	{
		switch(ev.getType())
		{
			//Exit Agent
			case RationalAgent.EXIT:
				this.doDelete();
				//System.exit(0);
				break;			
		}
	}
	
	/////////////////////////////////////////////
	// EVENT-HANDLING
	/////////////////////////////////////////////

	/**
	 * override jade.core.Agent.setup
	 */	
	@Override
	public void setup()
	{
		super.setup();
		
		// Register Codec
		m_Codec = new LEAPCodec();
   	 	getContentManager().registerLanguage( m_Codec );
   	 	
   	 	// Register Inter-Agent Ontology
   	 	m_Ontology = VSTOntology.getInstance();
    	getContentManager().registerOntology( m_Ontology );
    	
		// message receiving
		addBehaviour( new TracerReceiveBehaviour( this ));
		
		// register as service
		addBehaviour( new RegisterBehaviour( this, RationalAgent.TRACER_SERVICE ));
		
		//Create the Gui (delegated to all subclasses)
		createGui();

   		//Show the gui:
		myGui.setVisible(true);
		
		System.out.println("Tracer agent setup completed");
	}
	
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
}
