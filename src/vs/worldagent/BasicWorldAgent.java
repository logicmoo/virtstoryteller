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
package vs.worldagent;

import jade.core.AID;
import jade.domain.FIPANames;
import jade.gui.GuiEvent;
import jade.lang.acl.MessageTemplate;
import jade.proto.AchieveREResponder;

import java.util.logging.Logger;

import vs.Config;
import vs.communication.Operator;
import vs.communication.PerformOperator;
import vs.debug.LogFactory;
import vs.rationalagent.RationalAgent;
import vs.rationalagent.behaviour.RegisterBehaviour;
import vs.worldagent.behaviour.ReceiveInformBehaviour;
import vs.worldagent.behaviour.RequestResponderBehaviour;
import vs.worldagent.ui.WorldAgentGui;

/**
 * World Agent
 * 
 * @author swartjes Created on 20-jul-2005
 */
public class BasicWorldAgent extends RationalAgent implements IWorldAgent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 4813118502761861507L;

	// Events:
	public static final int SHOWGRAPH = 2000;

	public static final String PROLOG_FILE = "load_world.pl";

	private AID m_PlotAgent;

	// Time
	private int m_time;

	// The modules of this agent
	//private IWorldUpdateManager m_worldUpdateManager;
	private IOperatorScheduler m_operatorScheduler;

	private Logger logger;

	/**
	 * overwrites createGui() of superclass
	 */
	@Override
	protected void createGui() {
		myGui = new WorldAgentGui(this);
	}

	/* See IWorldAgent */
	public IOperatorScheduler getOperatorScheduler() {
		return m_operatorScheduler;
	}

	/* See IWorldAgent */
/*	public IWorldUpdateManager getWorldUpdateManager() {
		return m_worldUpdateManager;
	}*/

	/* See IWorldAgent */
	public AID getPlotAgent() {
		return m_PlotAgent;
	}

	/* See IWorldAgent */
	public int getTime() {
		return m_time;
	}

	/* See IWorldAgent */
	public boolean handlePerformOperator(PerformOperator po) {
		if (po != null) {
			Operator o = po.getOperator();
			return getOperatorScheduler().schedule(o);
		} else {
			logger.severe("Received a null operator!");
			return false;
		}
	}

	/**
	 * The ActorAgentGui sends GuiEvents, which are handled by onGuiEvent
	 */
	@Override
	protected void onGuiEvent(GuiEvent ev) {
		// let superclass handle event first
		super.onGuiEvent(ev);

		switch (ev.getType()) {
		case EXIT:
			this.doDelete();
			break;

		case SHOWGRAPH:
			// m_graphWindow = new GraphGui(new
			// LocationGraphFiller(getKnowledgeManager()));
			// ((WorldAgentGui)this.myGui).drawGraph();

			//JFrame jf = new JFrame();
			//JungTestPanel jtp = new JungTestPanel();
			//jf.getContentPane().add(jtp.createPanel());
			//jf.setJMenuBar(jtp.getMenuBar());

			// final GraphZoomScrollPane panel = new GraphZoomScrollPane(vv);
			// jf.getContentPane().add(jtp);
			//jf.pack();
			//jf.setVisible(true);

			break;
		}

	}

	/* See IWorldAgent */
	public void pulse(int roundNumber) {
		// Update everything. Should be triggered by Plot Agent

		m_time = roundNumber;
		((WorldAgentGui) myGui).writeStatus("Time: " + m_time);

		// Do updates needed to go to next time step
		m_operatorScheduler.pulse();

	}

	/* See IWorldAgent */
	public void setPlotAgent(AID agent) {
		m_PlotAgent = agent;
	}
	
	public void narrate(String txt) {
		((WorldAgentGui) myGui).narrate(txt);	
	}

	// HANDLE AGENT ACTIONS

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

		logger.info("Setting up the World Agent ...");

		// Initialize agent modules
		m_operatorScheduler = new BasicOperatorScheduler(this);
//		m_worldUpdateManager = new BasicWorldUpdateManager(this);

		// register this agent with DF
		addBehaviour(new RegisterBehaviour(this, RationalAgent.WORLD_SERVICE));

		// Add behaviour to respond to incoming REQUESTs
		MessageTemplate mt = AchieveREResponder
				.createMessageTemplate(FIPANames.InteractionProtocol.FIPA_REQUEST);
		addBehaviour(new RequestResponderBehaviour(this, mt));

		addBehaviour(new ReceiveInformBehaviour(this));

		myGui.setVisible(true);

		logger.info("Succesfully set up the World Agent");
		logger.info("Starting the world....");

		// Remove if done testing
		// kruizinga 13 feb 2007
		logger.info("Loading testing environment...");
		if (getKnowledgeManager().consult(
				Config.PROLOGFILESPATH + BasicWorldAgent.PROLOG_FILE)) {
			myGui.writeConsole("Succesfully consulted: "
					+ BasicWorldAgent.PROLOG_FILE);
		} else {
			myGui
					.writeConsole("Could not consult: "
							+ BasicWorldAgent.PROLOG_FILE);
		}

	}

}