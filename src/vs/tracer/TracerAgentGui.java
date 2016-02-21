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

import jade.gui.GuiEvent;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;

import vs.rationalagent.RationalAgent;

public class TracerAgentGui extends JFrame implements ActionListener
{
	/**
	 * windowEvent is necessary to detect the window closing by the user
	 */
    class windowEvents extends java.awt.event.WindowAdapter
    {
        @Override
		public void windowClosing(java.awt.event.WindowEvent event)
        {
	    	GuiEvent ev = new GuiEvent(null, RationalAgent.EXIT);
	    	m_Tracer.postGuiEvent(ev);
	    }
    }

	/**
	 * 
	 */
	private static final long serialVersionUID = -7468327446909264231L;

	//CONSTANTS
	private final static	int				FRAME_WIDTH		= 400;
	private final static	int				FRAME_HEIGHT	= 300;

	/////////////////////////////////////////////
	// ATTRIBUTES
	/////////////////////////////////////////////
	private TracerAgent			m_Tracer = null;
	//Components
	private		JTextField			m_cmdField;
	private		JTextArea			m_consoleArea;
	
	private		JScrollPane			m_consoleScrollPane;
	//Menu
	protected	JMenuBar			menuBar;
	
	private		JMenu				menu_File;
	
	private		JMenuItem			menuItem_File_Exit;
	
	/////////////////////////////////////////////
	// CONSTRUCTORS AND INSTANTIATION
	/////////////////////////////////////////////

	protected	JPanel				m_consolePanel;
	

	/////////////////////////////////////////////
	// SETUP
	/////////////////////////////////////////////

	public TracerAgentGui(TracerAgent a)
	{
		super();
		m_Tracer = a;

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
			m_Tracer.postGuiEvent( ev );
		} 
	}
	
	/////////////////////////////////////////////
	// METHODS
	/////////////////////////////////////////////

	/////////////////////////////////////////////
	// EVENT HANDLING
	/////////////////////////////////////////////

	/**
	 * layout() creates the layout of the gui
	 */
	protected void guiLayout()
	{
		//Frame settings
		setTitle(m_Tracer.getLocalName() + " (TracerAgentGui for " + m_Tracer.getClass() + ")");
		setSize(TracerAgentGui.FRAME_WIDTH, TracerAgentGui.FRAME_HEIGHT);
		
		setJMenuBar(menuBar);

		//Main panel
		JPanel main = new JPanel();
		main.setLayout( new BorderLayout() );
		main.add( m_consoleScrollPane, BorderLayout.CENTER );
	
		getContentPane().add(main,BorderLayout.CENTER);
	}	
    
    /**
	 * setup() creates the elements of the gui
	 * and adds event listeners
	 */
	protected void guiSetup()
	{
		//Menus	
		menuBar = new JMenuBar();

			//File menu
			menu_File = new JMenu("File");
			menu_File.getAccessibleContext().setAccessibleDescription("File Menu");
				menuItem_File_Exit = new JMenuItem("Exit");
			menu_File.add(menuItem_File_Exit);
		
		menuBar.add(menu_File);

		// Console
		m_consoleArea = new JTextArea( 20, 60 );
		m_consoleScrollPane = 
	    new JScrollPane(m_consoleArea,
	                    ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
	                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		m_consoleArea.setEditable(false);

		/////////////////////////////////////////
		// EVENT LISTENERS
		/////////////////////////////////////////
		
		this.addWindowListener(new windowEvents());

		menuItem_File_Exit.addActionListener( this );
	}
	
	public void writeConsole( final String message ) {
		writeConsole( message, true );
	}

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
