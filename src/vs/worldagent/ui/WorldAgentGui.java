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
package vs.worldagent.ui;

import jade.gui.GuiEvent;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Logger;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;

import vs.Config;
import vs.debug.LogFactory;
import vs.rationalagent.RationalAgent;
import vs.rationalagent.ui.RationalAgentGui;
import vs.rationalagent.ui.StoryAgentEvent;
import vs.worldagent.BasicWorldAgent;
import vs.worldagent.IWorldAgent;
import vs.worldagent.ScheduledOperator;

import com.hp.hpl.jena.shared.PrefixMapping;

/**
 * GUI for world agent
 * 
 * @author swartjes
 * Created on 20-sep-2005
 */
public class WorldAgentGui extends RationalAgentGui {
	
	/////////////////////////////////////////////
	// ATTRIBUTES
	/////////////////////////////////////////////
	
	class ColorCellRenderer extends DefaultTableCellRenderer{

	  @Override
	public Component getTableCellRendererComponent(JTable table, Object value,
	   boolean isSelected,boolean hasFocus,int row, int col){
	    
	    Component comp = super.getTableCellRendererComponent(table, value,
	     isSelected, hasFocus, row, col);
	  
	    if (isSelected == false && hasFocus == false){
	      ((JComponent)comp).setOpaque(true); //if comp is a JLabel
	      if(rowStatus.get(row) != null){ //you can specify arbitrary row
	    	Color c;
	    	  switch(rowStatus.get(row)) {
			case ScheduledOperator.ABORTED:
				c = Color.RED;
				break;
			case ScheduledOperator.CREATED:

				c = Color.gray;
				break;
			case ScheduledOperator.FINISHED:

				c = Color.GREEN;
				break;
			case ScheduledOperator.NO_LONGER_INTERRUPTABLE:

				c = Color.black;
				break;
			case ScheduledOperator.SCHEDULED:

				c = Color.black;				
				break;
			case ScheduledOperator.STARTED:

				c = Color.black;				
				break;				
			default:
				
				c = Color.gray;			
			}		    	

	        comp.setForeground(c);
	      }
	      else{
	        comp.setForeground(Color.orange);
	      }
	    }return comp;
	  }
	}

	/**
	 * 
	 */
	private static final long serialVersionUID = 4336263640707024737L;
	
	protected static String[] columnNames = 
    	{"Operator",
         "Type",
         "Agens",
         "Patiens",
         "Target",
         "Instrument",
         "Status"};
	// VARIABLES
	private final Logger logger;
	private	JTextArea m_infoArea;
	
	private JScrollPane m_infoScrollPane;
	private JLabel m_statusBar;
	
	private JMenu menu_WA;
	private	JMenuItem menuItem_WA_Graph;
	
	protected JPanel m_operatorPanel;
    
    //protected JTable m_operatorTable;
	protected JScrollPane m_operatorScrollPane;
	
	//CONSTANTS

	/////////////////////////////////////////////
	// CONSTRUCTORS AND INSTANTIATION
	/////////////////////////////////////////////

	protected HashMap<Integer,Integer> rowStatus;	
	
	/////////////////////////////////////////////
	// SETUP
	/////////////////////////////////////////////

	public WorldAgentGui(RationalAgent a)
	{
		super(a);
		rowStatus = new HashMap<Integer,Integer>();
		
		logger = LogFactory.getLogger(this);
		
		registerCommand( new ScheduleActionCommand( myAgent, this ));
		registerCommand( new PulseCommand( myAgent, this ));
		registerCommand( new ListActionScheduleCommand( myAgent, this ));
		//registerCommand( new ValidateActionCommand( (RationalAgent) myAgent, this));
		//registerCommand( new GetPossibleActionsCommand( (RationalAgent) myAgent, this));
		registerCommand( new TestCommand( myAgent, this));
		//registerCommand( new DoActionCommand( (RationalAgent) myAgent, this));
		registerCommand( new PulseOnCommand( myAgent, this));
		
		a.addEventListener(this);
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
		if(source == menuItem_WA_Graph) {
			GuiEvent ev = new GuiEvent(null, BasicWorldAgent.SHOWGRAPH);
			((BasicWorldAgent) myAgent).postGuiEvent( ev );
		} 
    	
	}
	

	
	
	/**
	 * layout() creates the layout of the gui
	 */
	@Override
	protected void guiLayout()
	{
		// same layout
		super.guiLayout();
		
		//Frame settings
		setTitle(myAgent.getLocalName() + " (WorldAgentGui for " + myAgent.getClass() + ")");
		setSize(getSize().width*2, getSize().height-50);
		
		 // Set icon
	    Image icon = Toolkit.getDefaultToolkit().getImage("img/wa_small.gif");
	    setIconImage(icon);
	    
		//setSize(FRAME_WIDTH, FRAME_HEIGHT);
		
		m_tabPane.add("Story Log", m_infoScrollPane);
	
		//m_tabPane.add("Knowledge Graph", f);	      
	}
		

	/////////////////////////////////////////////
	// EVENT HANDLING
	/////////////////////////////////////////////

	/**
	 * setup() creates the elements of the gui
	 * and adds event listeners
	 */
	@Override
	protected void guiSetup()
	{
		// WorldAgentGui is an extension of RationalAgentGui
		super.guiSetup();

		// World Agent menu
			menu_WA = new JMenu("World Agent");
				menuItem_WA_Graph = new JMenuItem("Show graph");
		
			menu_WA.add(menuItem_WA_Graph);
		menuBar.add(menu_WA);
			
		// Info area
		m_infoArea = new JTextArea(20,60);
		m_infoArea.setText("");
		
		m_infoScrollPane = 
		    new JScrollPane(m_infoArea,
		                    ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
		                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
			m_infoArea.setEditable(false);	
			
			
		m_operatorPanel = new JPanel(new BorderLayout());

		JTable operatorTable = makeOperatorTable();
		m_operatorScrollPane = new JScrollPane(operatorTable);
		m_operatorPanel.add(m_operatorScrollPane, BorderLayout.CENTER);
		//m_operatorPanel.add(m_operatorTable, BorderLayout.CENTER);
		m_tabPane.add(m_operatorPanel, "Operator Schedule");
			
		m_statusBar = new JLabel("Time: 0");
		m_controlPanel.add(m_statusBar, BorderLayout.SOUTH);
		        
        // Action listeners
		menuItem_WA_Graph.addActionListener( this );
	    		
	}
	
	public JTable makeOperatorTable() {
		
		DefaultTableModel tableModel = new DefaultTableModel();
		tableModel.setColumnIdentifiers(columnNames);
		
		
		if (((IWorldAgent) myAgent).getOperatorScheduler() == null) {
			return new JTable(tableModel);
		}
		
		List<ScheduledOperator> operators = ((IWorldAgent) myAgent).getOperatorScheduler().getScheduledOperators();
		
		PrefixMapping pm = PrefixMapping.Factory.create();
		pm.setNsPrefixes(Config.namespaceMap);
		
		for (ScheduledOperator o: operators) {
			String r1 = o.getOperator().getIndividual();
			String r2 = pm.shortForm(o.getOperator().getType());
			
			String r_a = "";
			if ((o.getOperator().getAgens() != null) && (o.getOperator().getAgens().charAt(0) != '_')) 
				r_a = pm.shortForm(o.getOperator().getAgens());
			
			String r_p = "";
			if ((o.getOperator().getPatiens() != null) && (o.getOperator().getPatiens().charAt(0) != '_'))
				r_p = pm.shortForm(o.getOperator().getPatiens());
			
			String r_t = "";
			if ((o.getOperator().getTarget() != null) && (o.getOperator().getTarget().charAt(0) != '_'))
				r_t = pm.shortForm(o.getOperator().getTarget());
			
			String r_i = "";
			if ((o.getOperator().getInstrument() != null) && (o.getOperator().getInstrument().charAt(0) != '_'))
				r_i = pm.shortForm(o.getOperator().getInstrument());
			
			String r3;
			Color c;
			rowStatus.put(tableModel.getRowCount(), o.getStatus());
			switch(o.getStatus()) {
			case ScheduledOperator.ABORTED:
				r3 = "ABORTED";
				c = Color.red;
				break;
			case ScheduledOperator.CREATED:
				r3 = "CREATED";
				c = Color.gray;
				break;
			case ScheduledOperator.FINISHED:
				r3 = "FINISHED";
				c = Color.GREEN;
				break;
			case ScheduledOperator.NO_LONGER_INTERRUPTABLE:
				r3 = "NO LONGER INTERRUPTABLE";
				c = Color.black;
				break;
			case ScheduledOperator.SCHEDULED:
				r3 = "SCHEDULED";
				c = Color.black;				
				break;
			case ScheduledOperator.STARTED:
				r3 = "STARTED";
				c = Color.black;				
				break;				
			default:
				r3 = "UNKNOWN";
				c = Color.MAGENTA;			
			}
			
			tableModel.addRow(new String[]{r1,r2, r_a, r_p, r_t, r_i,r3});
		
		}
		
		JTable operatorTable = new JTable(tableModel);
	    operatorTable.setDefaultRenderer(Object.class, new ColorCellRenderer());
		
		return operatorTable;
	}
	
    @Override
	public void onStoryAgentEvent(StoryAgentEvent e) {
		if (e instanceof OperatorsChangedEvent) {
			//new ListActionScheduleCommand( myAgent, this ).execute();
			m_operatorPanel.remove(m_operatorScrollPane);
			JTable operatorTable = makeOperatorTable();
			m_operatorScrollPane = new JScrollPane(operatorTable);
			m_operatorPanel.add(m_operatorScrollPane, BorderLayout.CENTER);
			repaint();
		}
	}	
	
		/////////////////////////////////////////////
		// METHODS
		/////////////////////////////////////////////
		public void writeStatus( final String message) {
			Runnable updateStatus = new Runnable() {			
				public void run() {
					m_statusBar.setText( message);
				}
			};
			
			SwingUtilities.invokeLater( updateStatus );
		}
		
		public void narrate(final String text) {
			Runnable updateStoryLog = new Runnable() {			
				public void run() {
					m_infoArea.append(text + "\n");
				}
			};
			
			SwingUtilities.invokeLater( updateStoryLog );
		}
	
}
