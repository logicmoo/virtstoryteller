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
package vs.rationalagent.ui;

import jade.gui.GuiEvent;

import java.awt.BorderLayout;
import java.awt.Desktop;
import java.awt.FileDialog;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;

import vs.debug.LogFactory;
import vs.rationalagent.RationalAgent;
import vs.rationalagent.StoryAgentEventListener;

public class RationalAgentGui extends JFrame implements ActionListener, StoryAgentEventListener
{
	/////////////////////////////////////////////
	// ATTRIBUTES
	/////////////////////////////////////////////

	protected class ExtendedAction extends AbstractAction {
		/**
		 * 
		 */
		private static final long serialVersionUID = -3297806278450601544L;

		public ExtendedAction(String text, ImageIcon icon, String desc, Integer mnemonic) {
	    		super(text, icon);
	    		putValue(Action.SHORT_DESCRIPTION, desc);
	    		putValue(Action.MNEMONIC_KEY, mnemonic);
	    		if (mnemonic != null) {
	    			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(mnemonic,
	    			    Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
	    		}
	    }
	    
		public void actionPerformed(ActionEvent e) {}
	}
	class ShowLogAction extends ExtendedAction {

		/**
		 * 
		 */

		public ShowLogAction(String text, ImageIcon icon, String desc,
				Integer mnemonic) {
			super(text, icon, desc, mnemonic);
		}

		@Override
		public void actionPerformed(ActionEvent e) {
						
			// Retrieve name of log file
			String logFileName = LogFactory.getIndex();

			// Open it
			try {
				URI u = new URI(logFileName);
				Desktop.getDesktop().browse(u);
			} catch (URISyntaxException use) {
				logger.warning("Could not parse log file name: " + logFileName);
			
			} catch (IOException io) {
				logger.warning("Could not open log file: " + logFileName);
			}
			
		}
	}
	/**
	 * windowEvent is necessary to detect the window closing by the user
	 */
    class windowEvents extends java.awt.event.WindowAdapter
    {
        @Override
		public void windowClosing(java.awt.event.WindowEvent event)
        {
	    	GuiEvent ev = new GuiEvent(null, RationalAgent.EXIT);
	    	(myAgent).postGuiEvent(ev);
	    }
    }
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -5524822911782233318L;
	//CONSTANTS
	private final static	int				FRAME_WIDTH		= 600;
	private final static	int				FRAME_HEIGHT	= 600;
	//VARIABLES
	protected	RationalAgent				myAgent;
	
	private		Map<String,Command>		m_commands;
	//Components
	private		JTextField			m_cmdField;
	private		JTextArea			m_consoleArea;
	protected	JTabbedPane			m_tabPane;	
	
	private		JScrollPane			m_consoleScrollPane;
	//Menu
	protected	JMenuBar			menuBar;
	private		JMenu				menu_File;
	private		JMenu				menu_KB;
	private		JMenu				menu_Debug;
	private		JMenuItem			menuItem_File_Exit;
	private		JMenuItem			menuItem_KB_Save;
	
	private		JMenuItem			menuItem_KB_Clear;
	private		JMenuItem			menuItem_KB_Load;
	
	private		JMenuItem			menuItem_KB_Consult;
	private		JMenuItem			menuItem_KB_Test;
	
	private		JMenuItem			menuItem_KB_LoadKifRules;
	protected	JPanel				m_consolePanel;

	/////////////////////////////////////////////
	// CONSTRUCTORS AND INSTANTIATION
	/////////////////////////////////////////////

	protected	JPanel				m_controlPanel;
	
	///////////////////////////////
	// GET & SET
	///////////////////////////////
	
	protected transient Action showLogAction;
	
	protected Logger logger;
	
	/////////////////////////////////////////////
	// SETUP
	/////////////////////////////////////////////

	public RationalAgentGui(RationalAgent a)
	{
		super();
		
		logger = LogFactory.getLogger(this);
		myAgent = a;

		m_commands = new HashMap<String,Command>();
		registerCommand( new TellCommand( myAgent, this ));
		registerCommand( new UntellCommand( myAgent, this ));
		registerCommand( new QueryCommand( myAgent, this ));
		registerCommand( new HelpCommand( myAgent, this ));
		registerCommand( new ListFactsCommand( myAgent, this ));
		registerCommand( new DisplayActionCommand( myAgent, this));
		//registerCommand( new CreatePrologActionDBCommand( (RationalAgent) myAgent, this));
		registerCommand( new PrologCommand( myAgent, this));
		
		// FIXME Strange, gui is based on an un-initialised agent, this way. 
		guiSetup();		
		guiLayout();		
	}

	/////////////////////////////////////////////
	// LAYOUT
	/////////////////////////////////////////////

	/**
     * action event handler
     */
	public void actionPerformed(ActionEvent e)
    {
		Object source = e.getSource();
		
		if(source == menuItem_File_Exit) {
			GuiEvent ev = new GuiEvent(null, RationalAgent.EXIT);
			(myAgent).postGuiEvent( ev );
		} 
		else if(source == menuItem_KB_Load) {
			String fileName = showFileDialog();
    		if (fileName != null) {
				GuiEvent ev = new GuiEvent(null, RationalAgent.LOAD_KB);
	    		ev.addParameter( fileName );
	    		(myAgent).postGuiEvent( ev );
    		}
		} 
		else if(source == menuItem_KB_Test) {
			GuiEvent ev = new GuiEvent(null, RationalAgent.TEST_KB);
    		(myAgent).postGuiEvent( ev );
		}
		else if ( source == menuItem_KB_Save ) {
			String fileName = showFileDialog();
    		if (fileName != null) {			
				GuiEvent ev = new GuiEvent( null, RationalAgent.SAVE_KB );
				ev.addParameter( fileName );
				(myAgent).postGuiEvent( ev );
    		}
		}
		else if ( source == menuItem_KB_Consult ) {
			String fileName = showFileDialog();
    		if (fileName != null) {			
				GuiEvent ev = new GuiEvent( null, RationalAgent.CONSULT );
				ev.addParameter( fileName );
				(myAgent).postGuiEvent( ev );
    		}
		}
		else if ( source == m_cmdField ) {
			Command cmd = parseCommand( m_cmdField.getText() );
			if ( cmd != null ) {
				getAgent().trace( 5, "executing command: " + cmd.getName());
				cmd.execute();
				writeConsole( "" );
			} else {
				writeConsole( "unrecognized command: " + m_cmdField.getText());
			}
			
			Runnable clearCmdField = new Runnable() {
				public void run() {
					m_cmdField.setText( "" );
				}
			};
			
			SwingUtilities.invokeLater(clearCmdField);
		}
	}
	
	/////////////////////////////////////////////
	// METHODS
	/////////////////////////////////////////////

	public RationalAgent getAgent()
	{
		return myAgent;
	}
	
	public String getCommandText()
	{
		return m_cmdField.getText();
	}

	/**
	 * layout() creates the layout of the gui
	 */
	protected void guiLayout()
	{
		//Frame settings
		setTitle(myAgent.getLocalName() + " (RationalAgentGui for " + myAgent.getClass() + ")");
		setSize(RationalAgentGui.FRAME_WIDTH, RationalAgentGui.FRAME_HEIGHT);
		
		setJMenuBar(menuBar);

		//Main panel
		JPanel main = new JPanel();
		main.setLayout( new BorderLayout() );
		main.add( m_controlPanel, BorderLayout.SOUTH );
		main.add( m_tabPane, BorderLayout.CENTER );
	
		getContentPane().add(main,BorderLayout.CENTER);
	}

	/**
	 * setup() creates the elements of the gui
	 * and adds event listeners
	 */
	protected void guiSetup()
	{
		// This GUI listens to its agent's events
		getAgent().addEventListener(this);
		
		showLogAction = new ShowLogAction("Show log", null, null, null);
		
		//Menus	
		menuBar = new JMenuBar();

			//File menu
			menu_File = new JMenu("File");
			menu_File.getAccessibleContext().setAccessibleDescription("File Menu");
				menuItem_File_Exit = new JMenuItem("Exit");
			menu_File.add(menuItem_File_Exit);
		
		menuBar.add(menu_File);

			// Knowledge-Base Menu
			menu_KB = new JMenu("Knowledge Base");
			menu_KB.getAccessibleContext().setAccessibleDescription("Knowledge Base Menu");
				menuItem_KB_Clear = new JMenuItem("Clear Knowledge Base");
				menuItem_KB_Save = new JMenuItem("Save KB");
				menuItem_KB_Load = new JMenuItem("Load RDF/OWL KB");
				menuItem_KB_Consult = new JMenuItem("Consult Prolog file");
				menuItem_KB_Test = new JMenuItem("Test KB");
				menuItem_KB_LoadKifRules = new JMenuItem("Load KIF Rules");
				
			menu_KB.add( menuItem_KB_Clear );
			menu_KB.add( menuItem_KB_Load );
			menu_KB.add( menuItem_KB_Consult );
			menu_KB.add( menuItem_KB_Test );
			menu_KB.add( menuItem_KB_LoadKifRules );
			menu_KB.add( menuItem_KB_Save );
			
		menuBar.add(menu_KB);
		
		menu_Debug = new JMenu("Debug");
		menu_Debug.add(showLogAction);
		
		menuBar.add(menu_Debug);

		// Control Panel
		m_controlPanel = new JPanel(new BorderLayout() );
		
			// Textfield (to enter commands)
			m_cmdField = new JTextField( 60 );
		m_controlPanel.add( m_cmdField, BorderLayout.NORTH );
		
		// Console
		m_consoleArea = new JTextArea( 20, 60 );
		m_consoleScrollPane = 
	    new JScrollPane(m_consoleArea,
	                    ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
	                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		m_consoleArea.setEditable(false);
		
		// Tabbed pane
		m_tabPane = new JTabbedPane();
		m_tabPane.add("Console", m_consoleScrollPane);

		/////////////////////////////////////////
		// EVENT LISTENERS
		/////////////////////////////////////////
		
		this.addWindowListener(new windowEvents());

		menuItem_File_Exit.addActionListener( this );
		menuItem_KB_Load.addActionListener( this );
		menuItem_KB_Consult.addActionListener( this );
		menuItem_KB_Test.addActionListener( this );
		menuItem_KB_LoadKifRules.addActionListener( this );
		menuItem_KB_Save.addActionListener( this );
		menuItem_KB_Clear.addActionListener( this );
		
//		m_tellButton.addActionListener( this );
//		m_askButton.addActionListener( this );
		
		m_cmdField.addActionListener( this );
	}
	
	/////////////////////////////////////////////
	// EVENT HANDLING
	/////////////////////////////////////////////

	public void onStoryAgentEvent(StoryAgentEvent e) {
    	// Do nothing.
    }	
    
    /**
	 * Parse a given command into command name and arguments
	 * @param commandline the line given as a command
	 * @return a parsed Command
	 */
	protected Command parseCommand( String commandline )
	{
		String cmdName;
		String args;
		int sepIdx = commandline.indexOf( ' ' );
		if ( sepIdx > -1 ) {
			cmdName = commandline.substring( 0, sepIdx ).trim();
			args = commandline.substring( sepIdx, commandline.length());
		}
		else {
			cmdName = commandline;
			args = "";
		}
		
		Command cmd = m_commands.get( cmdName );
		if ( cmd != null ) {
			cmd.parseCommandLine( args );
		}
		
		return cmd;
	}
    
    protected void registerCommand( Command cmd )
	{
		m_commands.put( cmd.getName(), cmd );
	}
	
	public boolean showCommand( String name )
	{
		Command cmd = m_commands.get( name );
		if ( cmd != null ) {
			writeConsole( "help for command " + cmd.getName() + ":" );
			writeConsole( cmd.getDescription());
			return true;
		}
			
		return false;
	}

	/**
	 * showFileDialog shows a load file dialog and returns the name of the file that was selected by the user
	 */
	protected String showFileDialog()
	{
        FileDialog fd = new FileDialog(this, "Load file", FileDialog.LOAD);
        fd.show();
        if(fd.getFile() == null)
		{
	    	return null;
		}
		return fd.getDirectory().concat(fd.getFile());
	}
	
	public void showRegisteredCommands()
	{
		for (String key: m_commands.keySet()) {
			Command cmd = m_commands.get(key);
			writeConsole( cmd.getName());
		}
		/*for ( Iterator it = m_commands.keySet().iterator(); it.hasNext(); ) {
			Object key = it.next();
			Command cmd = (Command) m_commands.get( key );
			
			writeConsole( cmd.getName());
		}*/
	}
	
	/**
	 * @see #writeConsole(String, boolean)
	 */
	public void writeConsole( final String message ) {
		writeConsole( message, true );
	}
	
	
	/**
	 * Writes message to the text console
	 * @param message the message
	 * @param bLineFeed whether to use a line feed after this message
	 */
	public void writeConsole( final String message, final boolean bLineFeed ) {
		Runnable updateCons = new Runnable() {
			
			public void run() {
				m_consoleArea.append( message );
				if ( bLineFeed ) {
					m_consoleArea.append( "\n" );
				}
			}
		};
		
		SwingUtilities.invokeLater( updateCons );
	}	
}