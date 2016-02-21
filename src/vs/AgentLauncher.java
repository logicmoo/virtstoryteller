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
package vs;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.PopupMenu;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Logger;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.ScrollPaneConstants;
import javax.swing.plaf.TabbedPaneUI;

import vs.debug.LogFactory;
import vs.utils.UniqueId;

/**
 * Launcher platform to start JADE, to run agents, and to see their output
 * 
 * @author swartjes
 *
 */
public class AgentLauncher extends JFrame implements ActionListener, MouseListener, ComponentListener {

	/**
	 * Thread that works like a pipe: writes from input stream to output stream
	 * 
	 * From http://forum.java.sun.com/thread.jspa?threadID=5125300&tstart=0
	 * @author swartjes
	 *
	 */
	class AsyncPipe  extends Thread
	{
	    private final OutputStream ostrm_;
	    
	    private final InputStream istrm_;
	    
	    public AsyncPipe(InputStream istrm, OutputStream ostrm)
	    {
	        istrm_ = istrm;
	        ostrm_= ostrm;
	    }
	    @Override
		public void run()
	    {
	        try
	        {
	            final byte[] buffer = new byte[1024];
	            for (int length = 0; (length = istrm_.read(buffer)) != -1;)
	            {
	                synchronized (ostrm_)
	                {
	                    ostrm_.write(buffer, 0, length);
	                }
	            }
	        }
	        catch (Exception e)
	        {
	            e.printStackTrace();
	        }
	    }
	}
	
	class CloseTabAction extends ExtendedAction {
		/**
		 * 
		 */
		private static final long serialVersionUID = 4072606754046794585L;

		public CloseTabAction(String text, ImageIcon icon, String desc, Integer mnemonic) {
			super(text, icon, desc, mnemonic);
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			super.actionPerformed(e);
			if (tabPane.getSelectedComponent() != mainTabComponent) {
				tabPane.remove(tabPane.getSelectedComponent());
			}
	    }        		
	}
	
	class ExtendedAction extends AbstractAction {
		/**
		 * 
		 */
		private static final long serialVersionUID = 374554584892620077L;

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
	
	/**
	 * To pipe output towards JTextArea
	 * 
	 * From http://forum.java.sun.com/thread.jspa?threadID=771727&messageID=4397099
	 * @author swartjes
	 *
	 */
	public class TextAreaOutputStream extends OutputStream{
		 
		  private JTextArea textArea;
		  private StringBuilder sb;
		  
		  public TextAreaOutputStream(JTextArea textArea){
		    this.textArea = textArea;
		    sb = new StringBuilder();
		  }
		 
		  @Override
		public void close()throws IOException{
		    this.textArea = null;
		  }
		  
		  @Override
		public void write(int b)throws IOException{
			  
				if (b == '\r')
					return;
				
				if (b == '\n') {					
					textArea.append(sb.toString());
					sb.setLength(0);
					this.textArea.setCaretPosition(textArea.getDocument().getLength());
				}
				
				sb.append((char)b);			  
			  
		    //this.textArea.append(Character.toString((char)b));
			
		  }
		 
		}
	
	private static final long serialVersionUID = -2094331996376189092L;
	protected static Map<String,String> agentClasses = new HashMap<String,String>();
	static {
		AgentLauncher.agentClasses.put("World agent", "vs.worldagent.BasicWorldAgent");
		AgentLauncher.agentClasses.put("Plot agent", "vs.plotagent.BasicPlotAgent");
		AgentLauncher.agentClasses.put("Character agent", "vs.characteragent.BasicCharacterAgent");
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {

		AgentLauncher vst = new AgentLauncher();
	}
	
	//protected JTextField agentNameField;
	protected JComboBox chooseAgentClassBox;
	
	protected JComboBox chooseStoryDomainBox;
	protected JButton startJadeButton;
	protected JButton startAgentButton;
	protected JButton closeTabsButton;
	
	protected JButton killAgentsButton;
	protected JPanel agentButtonPanel;
	
	protected Set<JButton> agentButtonSet; 
	
	protected JTabbedPane tabPane;
	
	protected JTextArea jadeArea;
	protected JScrollPane jadeScroll;
	
	//protected static String blank = "<let system decide>";
	
	protected JPopupMenu popupMenu;
	protected JLabel imgLabel;
	
	protected JEditorPane domDesc;
	
	protected JComponent mainTabComponent;
	
	
	protected Map<String, StoryDomain> _storyDomains;
		
	protected Properties _properties;
	
	protected Properties _dynaProperties;
	
	protected transient Action closeTabAction;
	
	protected Logger logger;

 	public AgentLauncher() {
		
		super("Virtual Storyteller - Agent Launcher"); 
		
		logger = LogFactory.getLogger(this);
		
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		_storyDomains = new HashMap<String, StoryDomain>();
		fillStoryDomains();
		
		imgLabel = new JLabel();
		imgLabel.setIcon(new ImageIcon("img/VST2.gif"));
			
		 // Set icon
	    Image icon = Toolkit.getDefaultToolkit().getImage("img/vs_small.gif");
	    setIconImage(icon);
		
			
		//chooseAgentClassBox = new JComboBox(AgentLauncher.agentClasses.keySet().toArray());
		agentButtonPanel = new JPanel();
		agentButtonPanel.setLayout(new BoxLayout(agentButtonPanel, BoxLayout.Y_AXIS));
		agentButtonPanel.setBorder(BorderFactory.createEmptyBorder(5, 10, 5, 10));
		agentButtonSet = new HashSet<JButton>();
		//Vector<JButton> agentButtons = new Vector<JButton>();
		JButton waButton = new JButton("World agent", new ImageIcon("img/wa_small.gif"));
		waButton.setMaximumSize(new Dimension(160,25));
		waButton.addActionListener(this);
		JButton caButton = new JButton("Character agent", new ImageIcon("img/ca_small.gif"));
		caButton.setMaximumSize(new Dimension(160,25));
		caButton.addActionListener(this);
		JButton paButton = new JButton("Plot agent", new ImageIcon("img/pa_small.gif"));
		paButton.setMaximumSize(new Dimension(160,25));
		paButton.addActionListener(this);
		
		agentButtonPanel.add(paButton);
		agentButtonSet.add(paButton);
		agentButtonPanel.add(Box.createRigidArea(new Dimension(0,5)));
		agentButtonSet.add(waButton);
		agentButtonPanel.add(waButton);
		agentButtonPanel.add(Box.createRigidArea(new Dimension(0,5)));
		agentButtonPanel.add(caButton);
		agentButtonSet.add(caButton);
		
/*		
		for (String agName: agentClasses.keySet() ) {
			JButton agButton = new JButton(agName);
			agButton.setMaximumSize(new Dimension(160,25));
			agButton.addActionListener(this);
			agentButtonPanel.add(agButton);
			agentButtonPanel.add(Box.createRigidArea(new Dimension(0,5)));
			agentButtonSet.add(agButton);
			
		}
		*/
		/*
		 * Agent control panel - for selecting story domain and starting agents
		 */
		JPanel storyDomainPanel = new JPanel(new BorderLayout());
		JPanel descSelectorPanel = new JPanel(new BorderLayout());
		
		
		domDesc = new JEditorPane();
		domDesc.setPreferredSize(new Dimension(300,80));
		domDesc.setContentType("text/html");
		//domDesc.setLineWrap(true);
		//domDesc.setColumns(20);
		domDesc.setEditable(false);
		JScrollPane domScrollPane = new JScrollPane(domDesc);
		domScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
		descSelectorPanel.setBorder(BorderFactory.createEmptyBorder(5, 10, 5, 10));
		

		chooseStoryDomainBox = new JComboBox(_storyDomains.keySet().toArray());
		chooseStoryDomainBox.addActionListener(this);
		
		descSelectorPanel.add(chooseStoryDomainBox, BorderLayout.CENTER);
		descSelectorPanel.add(new JLabel("Story domain: "), BorderLayout.WEST);
		
		storyDomainPanel.add(descSelectorPanel, BorderLayout.NORTH);

		//storyDomainPanel.add(chooseStoryDomainBox, BorderLayout.CENTER);
		String currDomain = _dynaProperties.getProperty("current_story_domain");
		if (currDomain != null) {
			chooseStoryDomainBox.setSelectedItem(currDomain);
		}
		String domainName = (String)chooseStoryDomainBox.getSelectedItem();
		StoryDomain sd = _storyDomains.get(domainName);
		domDesc.setText(sd.toString());
		domDesc.setCaretPosition(0);

		storyDomainPanel.add(domScrollPane, BorderLayout.CENTER);
		storyDomainPanel.setBorder(BorderFactory.createEmptyBorder(5, 10, 5, 10));
		

		/*
		 * JADE control panel - for starting JADE and killing agents
		 */
		JPanel JADEcontrol = new JPanel();
		JADEcontrol.setLayout(new BoxLayout(JADEcontrol, BoxLayout.Y_AXIS));
		closeTabsButton = new JButton("Clear tabs");
		closeTabsButton.setMaximumSize(new Dimension(160,25));
		closeTabsButton.addActionListener(this);
		
		startJadeButton = new JButton("Start JADE", new ImageIcon("img/jadelogosmall.jpg"));
		startJadeButton.setMaximumSize(new Dimension(160,25));
		startJadeButton.addActionListener(this);
		
		killAgentsButton = new JButton("Kill agents", new ImageIcon("img/ninja.jpg"));
		killAgentsButton.setMaximumSize(new Dimension(160,25));
		killAgentsButton.addActionListener(this);
		
		JADEcontrol.add(startJadeButton);
		JADEcontrol.add(Box.createRigidArea(new Dimension(0,5)));		
		JADEcontrol.add(killAgentsButton);
		JADEcontrol.add(Box.createRigidArea(new Dimension(0,5)));		
		JADEcontrol.add(closeTabsButton);
		JADEcontrol.setBorder(BorderFactory.createEmptyBorder(5, 10, 5, 10));

		
	
		// CREATE POPUP MENU
		closeTabAction = new CloseTabAction("Close tab", null, null, null);
		popupMenu = new JPopupMenu();
		popupMenu.add(closeTabAction);
		
	
		// MAKE THE PANE WITH THE CONTROL BUTTONS
			
		JPanel imgPanel = new JPanel(new BorderLayout());
		imgPanel.add(imgLabel);
		
		JPanel controlPane = new JPanel(new BorderLayout());
	
		controlPane.add(storyDomainPanel, BorderLayout.CENTER);
		
		controlPane.add(JADEcontrol, BorderLayout.WEST);
		controlPane.add(agentButtonPanel, BorderLayout.EAST);
		

		JPanel topPane = new JPanel(new BorderLayout());
		topPane.setPreferredSize(new Dimension(800,140));
		topPane.add(controlPane, BorderLayout.CENTER); //, BorderLayout.CENTER
		//topPane.add(agentButtonPanel); // , BorderLayout.EAST
		topPane.add(imgPanel, BorderLayout.EAST); // , BorderLayout.EAST

		// MAKE THE PANE WITH THE OUTPUTS
		tabPane = new JTabbedPane();
		tabPane.setPreferredSize(new Dimension(800, 500));
		tabPane.addMouseListener(this);

	
		jadeArea = new JTextArea(40,40);
		jadeScroll = new JScrollPane(jadeArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		tabPane.add(jadeScroll, "JADE platform");
		mainTabComponent = jadeScroll;
		
		PopupMenu popup = new PopupMenu("test");
		jadeScroll.add(popup);
		
		

		// PUT TOGETHER THE WHOLE THING
		JPanel contentPane = new JPanel();
		contentPane.setLayout(new BorderLayout());
		
		contentPane.add(topPane, BorderLayout.NORTH);
		contentPane.add(tabPane, BorderLayout.CENTER);
		
		this.setMinimumSize(new Dimension(670,250));
		this.setContentPane(contentPane);
		this.pack();
		this.setVisible(true);
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		
	}
 	
 	public void actionPerformed(ActionEvent e) {

		if (e.getSource() == startJadeButton) {
			String execStr = Config.RUN_PROCESS_BATCHFILE + " jade.Boot -gui sniffy:jade.tools.sniffer.Sniffer";
			
			System.out.println("Executing: cmd /c " + execStr);
			Process p = execProcess("cmd", "/C", execStr);

			new AsyncPipe(p.getErrorStream(), new TextAreaOutputStream(jadeArea)).start();
	        new AsyncPipe(p.getInputStream(), System.out).start();
			
			
		}
		
		if (agentButtonSet.contains(e.getSource())) {
			JButton src = (JButton)e.getSource();
			String name;
				
			String classStr = AgentLauncher.agentClasses.get(src.getText());
			if (classStr == null) {
				// This should not happen
				logger.severe("Agent class " + classStr + " not found!");
			}
			
			name = UniqueId.generateUniqueAgentName(
					src.getText().substring(0, 1), "agent");				
			
			launchAgent(name, classStr);
			
		}
		
		
		if (e.getSource() == closeTabsButton) {
			tabPane.removeAll();
			tabPane.add(jadeScroll, "JADE platform");
		}
		
		if (e.getSource() == killAgentsButton) {
			String execStr = Config.RUN_PROCESS_BATCHFILE + " jade.Boot -container assassin:vs.AssassinAgent";
			
			System.out.println("Executing: cmd /c " + execStr);
			Process p = execProcess("cmd", "/C", execStr);

			new AsyncPipe(p.getErrorStream(), new TextAreaOutputStream(jadeArea)).start();
	        new AsyncPipe(p.getInputStream(), System.out).start();

		}
		
		if (e.getSource() == chooseStoryDomainBox) {

			String domainName = (String)chooseStoryDomainBox.getSelectedItem();
			StoryDomain sd = _storyDomains.get(domainName);
			domDesc.setText(sd.toString());
			domDesc.setCaretPosition(0);
			_dynaProperties.setProperty("current_story_domain", domainName);
			
			try {
				FileOutputStream fos = new FileOutputStream(Config.PROPERTIES_STATEFILE);
				_dynaProperties.store(fos, null);
				fos.close();			    
			    
			} catch (IOException eo) {
				logger.severe("Cannot write to properties file " + Config.PROPERTIES_STATEFILE);
				eo.printStackTrace();
			}
			
		}

	}
 	public void componentHidden(ComponentEvent e) {}
 	public void componentMoved(ComponentEvent e) {}
 	
 	public void componentResized(ComponentEvent e) {}
 	
 	/**
 	 * Fires when a certain sheet is displayed. Used to fill the pull down menus with the right Actions.
 	 */
 	public void componentShown(ComponentEvent e) {
 		
 	} 	
 	public Process execProcess(String... args) {
		Process p; 
		ProcessBuilder pb;
		
		try {
			
			pb = new ProcessBuilder(args);
			p = pb.start();
		
	       return p;


		} catch(Exception e) {
			e.printStackTrace();
			return null;
		}
	}
 	
 	public void fillStoryDomains() {
		try {
			_properties = new Properties();
			FileInputStream fis = new FileInputStream(Config.PROPERTIESFILE);
			_properties.load(fis);
			fis.close();
			
			_dynaProperties = new Properties();
			FileInputStream fis2 = new FileInputStream(Config.PROPERTIES_STATEFILE);
			_dynaProperties.load(fis2);
			fis2.close();
		    
		    // Read in story domain
		    Enumeration<String> en = (Enumeration<String>) _properties.propertyNames();
		    while (en.hasMoreElements()) {
		    	String key = en.nextElement();
		    	if (key.startsWith("story_domain") && key.endsWith("name")) {
		    		// We're dealing with the name of a story domain
		    		String name = _properties.getProperty(key);
		    		String author = _properties.getProperty("story_domain." + name + ".author");
		    		String desc = _properties.getProperty("story_domain." + name + ".desc");
		    		String ont = _properties.getProperty("story_domain." + name + ".ontology");
		    		String alt = _properties.getProperty("story_domain." + name + ".ontology.local");
		    		StoryDomain sd = new StoryDomain(name, author, desc, ont, alt);
		    		_storyDomains.put(name, sd);
		    	}
		    }
		    
		    
		} catch (IOException e) {
			logger.severe("Cannot read properties file " + Config.PROPERTIESFILE + " or " + Config.PROPERTIES_STATEFILE);
			e.printStackTrace();
		}
	}
 	public void launchAgent(String name, String classStr) {
		
		String execStr = Config.RUN_PROCESS_BATCHFILE + " jade.Boot -container " + name + ":" + classStr;
		System.out.println("Executing: cmd /c " + execStr);

		Process p = execProcess("cmd", "/C", execStr);
		
		JTextArea newArea = new JTextArea(40,40);
		JScrollPane newScroll = new JScrollPane(newArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		
		tabPane.add(newScroll, name + ":" + classStr);
		tabPane.setSelectedComponent(newScroll);
		
        new AsyncPipe(p.getErrorStream(), new TextAreaOutputStream(newArea)).start();
        new AsyncPipe(p.getInputStream(), System.out).start();
        //new AsyncPipe(p.getOutputStream(), System.out).start();
	}
 	

	private void maybeShowPopup(MouseEvent e, JTabbedPane src)
    {

       if (e.isPopupTrigger())
       {
          // see if the click was in one of tabs
          int tabcount = src.getTabCount();
          for(int i=0;i<tabcount;i++)
          {
             TabbedPaneUI tpu = src.getUI();
             Rectangle rect = tpu.getTabBounds(src, i);
             int x = e.getX();
             int y = e.getY();
             if (x < rect.x  ||  x > rect.x+rect.width  ||  y < rect.y  ||  y > rect.y+rect.height) {
				continue;
			}

             //setSelectedIndex shouldn't be necessary here, but just in case the listeners get 
             // called in different order (?), the callback code for each my menu items assumes 
             // that it can use getSelectedIndex() to determine which tab it should operate upon.
             src.setSelectedIndex(i);

             popupMenu.show(e.getComponent(), e.getX(), e.getY());
             break;
          }
       }
    }
	
    public void mouseClicked(MouseEvent e) {}
	
	
	public void mouseEntered(MouseEvent e) {}	
	
	public void mouseExited(MouseEvent e) {}	
	
	public void mousePressed(MouseEvent e) {
		maybeShowPopup(e, tabPane);
 	}	
	
	public void mouseReleased(MouseEvent e) {
 		maybeShowPopup(e, tabPane); 		
 	}	


}