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
package vs.characteragent.ui;

import jade.gui.GuiAgent;
import jade.gui.GuiEvent;

import java.awt.Image;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.logging.Logger;

import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTree;

import vs.characteragent.BasicCharacterAgent;
import vs.characteragent.ICharacterAgent;
import vs.debug.LogFactory;
import vs.fabula.io.LanguageFilter;
import vs.rationalagent.RationalAgent;
import vs.rationalagent.ui.RationalAgentGui;
import vs.worldagent.BasicWorldAgent;

public class CharacterAgentGui extends RationalAgentGui {

	/**
	 * 
	 */
	private static final long serialVersionUID = -1655976715125777663L;
	
	/////////////////////////////////////////////
	// ATTRIBUTES
	/////////////////////////////////////////////
	
	// VARIABLES
	private Logger logger;
	
	// private JScrollPane m_infoScrollPane;
	private PlanGraphPanel graphPane;
	private JMenu menu_CA;
	
	//private	JButton refreshMind;
	protected JPanel m_mindTreePanel;
	
	protected JTree m_mindTree;
	
	protected transient Action saveEpisodicMemoryAction;
	protected transient Action planAction;
		
	//CONSTANTS

	/////////////////////////////////////////////
	// CONSTRUCTORS AND INSTANTIATION
	/////////////////////////////////////////////

	protected JFrame frame = this;
	
	/////////////////////////////////////////////
	// SETUP
	/////////////////////////////////////////////

	public CharacterAgentGui(RationalAgent a)
	{
		super(a);
		
		logger = LogFactory.getLogger(this);
		
		registerCommand( new GetAgentIDCommand( myAgent, this ));
	}
	
	/////////////////////////////////////////////
	// LAYOUT
	/////////////////////////////////////////////

	/**
     * action event handler
     */
	@Override
	public void actionPerformed(ActionEvent e)
    {
    	// give superclass opportunity to handle event
    	super.actionPerformed( e );
    	Object source = e.getSource();
				
    }
	
	/////////////////////////////////////////////
	// METHODS
	/////////////////////////////////////////////

	public PlanGraphPanel getGraphPane() {
		if (graphPane == null && ((ICharacterAgent)myAgent).getCharacterProcess().getDeliberativeLayer() != null) {
			graphPane = new PlanGraphPanel(((ICharacterAgent)myAgent).getCharacterProcess().getDeliberativeLayer().getPlanner());
       		m_tabPane.add("Plan Graph", graphPane);
		} 
		return graphPane;
	}

	
	/////////////////////////////////////////////
	// EVENT HANDLING
	/////////////////////////////////////////////

    /**
	 * layout() creates the layout of the gui
	 */
	@Override
	protected void guiLayout()
	{
		super.guiLayout();
		
		//Frame settings
		setTitle(myAgent.getLocalName() + " (CharacterAgentGui for " + myAgent.getClass() + ")");
		this.setLocationRelativeTo(null);
				
		 // Set icon
	    Image icon = Toolkit.getDefaultToolkit().getImage("img/ca_small.gif");
	    setIconImage(icon);
			
	}
	
	/**
	 * setup() creates the elements of the gui
	 * and adds event listeners
	 */
	@Override
	protected void guiSetup()
	{
		// CharacterAgentGui is an extension of RationalAgentGui
		super.guiSetup();
		
		saveEpisodicMemoryAction = new SaveEpisodicMemoryAction("Save episodic memory", null, null, null);
		planAction = new PlanAction("Make plan for current goal", null, null, null);
	
		// Character Agent menu
			menu_CA = new JMenu("Character Agent");
				
			menu_CA.add(saveEpisodicMemoryAction);
			menu_CA.add(planAction);
			
		menuBar.add(menu_CA);
		        
		m_mindTreePanel = new MindDisplayer((BasicCharacterAgent) myAgent);				
	    m_tabPane.add("Mind", m_mindTreePanel);
	}

	class SaveEpisodicMemoryAction extends ExtendedAction {

		/**
		 * 
		 */
		private static final long serialVersionUID = 5681926205350781775L;

		public SaveEpisodicMemoryAction(String text, ImageIcon icon, String desc,
				Integer mnemonic) {
			super(text, icon, desc, mnemonic);
		}

		@Override
		public void actionPerformed(ActionEvent e) {

			JFileChooser chooser = new JFileChooser();
			LanguageFilter trigFilter = new LanguageFilter("TriG",
					".trig", "TRIG", false);	
			
			LanguageFilter trixFilter = new LanguageFilter("TriX",
					".trix", "TRIX", false);
			//File f = new File ("");
			//chooser.setSelectedFile(f);
			chooser.setAcceptAllFileFilterUsed(false);

			chooser.setFileFilter(trixFilter);
			chooser.setFileFilter(trigFilter);

			File returnFile = LanguageFilter.showSaveDialog(chooser, frame);
			String language = null;

			if (returnFile != null) {
	
				LanguageFilter filter = (LanguageFilter) chooser.getFileFilter();
				if (filter == trigFilter || filter == trixFilter) {
					language = filter.getDescription();
			
					GuiEvent ev = new GuiEvent(null, BasicCharacterAgent.SAVEEPISODICMEMORY);
					ev.addParameter(returnFile);
					ev.addParameter(filter);
					((GuiAgent) myAgent).postGuiEvent(ev);

				}

				
			}
			
			
		}
	}
	
	class PlanAction extends ExtendedAction {

		/**
		 * 
		 */
		private static final long serialVersionUID = 5681926205350781775L;

		public PlanAction(String text, ImageIcon icon, String desc,
				Integer mnemonic) {
			super(text, icon, desc, mnemonic);
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			GuiEvent ev = new GuiEvent(null, BasicCharacterAgent.CREATEPLAN);
			((GuiAgent) myAgent).postGuiEvent(ev);
		}
	}	
	
}
