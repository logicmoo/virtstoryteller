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

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Enumeration;
import java.util.Iterator;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

import vs.Config;
import vs.characteragent.ActorProcess;
import vs.characteragent.AdoptedGoalSchema;
import vs.characteragent.ExecutionState;
import vs.characteragent.ICharacterAgent;
import vs.characteragent.JustifiableGoalSchema;
import vs.communication.FramingOperator;
import vs.communication.Operator;
import vs.communication.StoryAction;
import vs.communication.StoryEvent;
import vs.rationalagent.ui.UpdateablePanel;

import com.hp.hpl.jena.shared.PrefixMapping;

public class MindDisplayer extends UpdateablePanel {
	
	class ActionNode extends DefaultMutableTreeNode {
		public ActionNode(Object name) {
			super(name);
		}		
	}
	class EventNode extends DefaultMutableTreeNode {
		public EventNode(Object name) {
			super(name);
		}		
	}
	class FramingNode extends DefaultMutableTreeNode {
		public FramingNode(Object name) {
			super(name);
		}		
	}
	class GoalNode extends DefaultMutableTreeNode {
		protected AdoptedGoalSchema schema;
		
		public GoalNode(Object name, AdoptedGoalSchema gs) {
			super(name);
			schema = gs;
		}
		
		public AdoptedGoalSchema getAdoptedGoalSchema() {
			return schema;
		}
	}
	
	class JustifiableGoalNode extends DefaultMutableTreeNode {
		protected JustifiableGoalSchema schema;
		
		public JustifiableGoalNode(Object name, JustifiableGoalSchema gs) {
			super(name);
			schema = gs;
		}
		
		public JustifiableGoalSchema getJustifiableGoalSchema() {
			return schema;
		}
	}
	
	class MindTreeRenderer extends DefaultTreeCellRenderer {

		ImageIcon goalIcon = new ImageIcon("img/goal.gif");
		ImageIcon actionIcon = new ImageIcon("img/action.gif");
		ImageIcon eventIcon = new ImageIcon("img/event.gif");
		ImageIcon framingIcon = new ImageIcon("img/framing.gif");
		ImageIcon jGoalIcon = new ImageIcon("img/j_goal.gif");
	    
	    public MindTreeRenderer() {
	    }

	    @Override
		public Component getTreeCellRendererComponent(
	                        JTree tree,
	                        Object value,
	                        boolean sel,
	                        boolean expanded,
	                        boolean leaf,
	                        int row,
	                        boolean hasFocus) {

	        super.getTreeCellRendererComponent(
	                        tree, value, sel,
	                        expanded, leaf, row,
	                        hasFocus);
	        
        	if (value instanceof GoalNode) {
        		setIcon(goalIcon);	 
        	}
        	
        	if (value instanceof JustifiableGoalNode) {
        		setIcon(jGoalIcon);	 
        	}
        	
        	if (value instanceof EventNode) {
        		setIcon(eventIcon);	        		
        	}
        	
        	if (value instanceof ActionNode) {
        		setIcon(actionIcon);	        		
        	}
        	
        	if (value instanceof FramingNode) {
        		setIcon(framingIcon);	        		
        	}        	
        	
        	this.setText(value.toString());

	        return this;
	    }	
	}
	
	class ShowPlanGraphAction extends AbstractAction {

		/**
		 * 
		 */
		protected AdoptedGoalSchema goal;

		public ShowPlanGraphAction(String text, ImageIcon icon, AdoptedGoalSchema gs) {
			super(text, icon);
			goal = gs;
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			JFrame goalFrame = new JFrame("Plan graph for goal " + pm.shortForm(goal.getGoalSchema().getType()));
			PlanGraphPanel goalGraphPanel = new PlanGraphPanel(goal.getPlanner());
			goalGraphPanel.refresh();
			goalFrame.setContentPane(goalGraphPanel);
			goalFrame.pack();
			goalFrame.show();
		}
	}
	
	class ShowJustifyingPlanGraphAction extends AbstractAction {

		/**
		 * 
		 */
		protected JustifiableGoalSchema goal;

		public ShowJustifyingPlanGraphAction(String text, ImageIcon icon, JustifiableGoalSchema gs) {
			super(text, icon);
			goal = gs;
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			JFrame goalFrame = new JFrame("Plan graph for justification of goal " + pm.shortForm(goal.getGoalSchema().getType()));
			PlanGraphPanel goalGraphPanel = new PlanGraphPanel(goal.getJustifyingPlanner());
			goalGraphPanel.refresh();
			goalFrame.setContentPane(goalGraphPanel);
			goalFrame.pack();
			goalFrame.show();
		}
	}
	
	class RePlanAction extends AbstractAction {

		/**
		 * 
		 */
		protected AdoptedGoalSchema goal;

		public RePlanAction(String text, ImageIcon icon, AdoptedGoalSchema gs) {
			super(text, icon);
			goal = gs;
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			goal.getPlanner().plan();
		}
	}	
	
	protected ICharacterAgent m_characterAgent;
	
	protected JTree mindTree;

	protected JPopupMenu popup;
	
	
	protected PrefixMapping pm;
	
	public MindDisplayer(ICharacterAgent character) {
		super();
		m_characterAgent = character;
		pm = PrefixMapping.Factory.create();
		pm.setNsPrefixes(Config.namespaceMap);
		update();
	}
	
	protected JTree createMindTree() {
		
		DefaultMutableTreeNode root = new DefaultMutableTreeNode("Mind");
		mindTree = new JTree(root);
		mindTree.setCellRenderer(new MindTreeRenderer());
		
		mindTree.setExpandsSelectedPaths(true);
		String roleDesc = m_characterAgent.getCharacterURI();
		if (roleDesc == null) {
			roleDesc = "no role (yet).";
		}
		DefaultMutableTreeNode role = new DefaultMutableTreeNode("Role: " + pm.shortForm(roleDesc));
	
		DefaultMutableTreeNode goals = new DefaultMutableTreeNode("Goals");		
		
		if (m_characterAgent.getCharacterProcess() != null) {
	
			AdoptedGoalSchema activeGoal = m_characterAgent.getCharacterProcess().getDeliberativeLayer().getActiveGoal();
			
			DefaultMutableTreeNode activeGoalNode;
			
			if (activeGoal != null) {
				activeGoalNode = new GoalNode("ACTIVE " + pm.shortForm(activeGoal.getGoalSchema().getType()), activeGoal);
				DefaultMutableTreeNode prolog = new DefaultMutableTreeNode(activeGoal.getGoalSchema().getPrologDescription());
				activeGoalNode.add(prolog);	
				DefaultMutableTreeNode urgency = new DefaultMutableTreeNode("Urgency: " + activeGoal.getUrgency());
				activeGoalNode.add(urgency);
				DefaultMutableTreeNode suggested = new DefaultMutableTreeNode("Suggested: " + activeGoal.getSuggested());
				activeGoalNode.add(suggested);
	
			} else {
				activeGoalNode = new DefaultMutableTreeNode("No active goal.");
			}
	
			goals.add(activeGoalNode);
			
			for (AdoptedGoalSchema ags: m_characterAgent.getCharacterProcess().getDeliberativeLayer().getGoals()) {			
				if (ags != activeGoal ) {
					GoalNode goal = new GoalNode(pm.shortForm(ags.getGoalSchema().getType()), ags);
					DefaultMutableTreeNode prolog2 = new DefaultMutableTreeNode(ags.getGoalSchema().getPrologDescription());
					goal.add(prolog2);
					DefaultMutableTreeNode urgency = new DefaultMutableTreeNode("Urgency: " + ags.getUrgency());
					goal.add(urgency);
					DefaultMutableTreeNode suggested = new DefaultMutableTreeNode("Suggested: " + ags.getSuggested());
					goal.add(suggested);
					goals.add(goal);
				}
			}
		}
		
		DefaultMutableTreeNode just_goals = new DefaultMutableTreeNode("Justifiable goals");
		
		if (m_characterAgent.getCharacterProcess() != null &&
				m_characterAgent.getCharacterProcess() instanceof ActorProcess) {
			ActorProcess actor = (ActorProcess)m_characterAgent.getCharacterProcess();
	
			JustifiableGoalSchema activeGoal = actor.getDeliberativeLayer().getActiveJustifiableGoal();
			
			DefaultMutableTreeNode activeGoalNode;
			
			if (activeGoal != null) {
				activeGoalNode = new JustifiableGoalNode("ACTIVE " + pm.shortForm(activeGoal.getGoalSchema().getType()), activeGoal);
				DefaultMutableTreeNode prolog = new DefaultMutableTreeNode(activeGoal.getGoalSchema().getPrologDescription());
				activeGoalNode.add(prolog);	
				DefaultMutableTreeNode suggested = new DefaultMutableTreeNode("Suggested: " + activeGoal.getSuggested());
				activeGoalNode.add(suggested);
	
			} else {
				activeGoalNode = new DefaultMutableTreeNode("No active justifiable goal.");
			}
	
			just_goals.add(activeGoalNode);
			
			for (JustifiableGoalSchema ags: actor.getDeliberativeLayer().getJustifiableGoals()) {			
				if (ags != activeGoal ) {
					JustifiableGoalNode goal = new JustifiableGoalNode(pm.shortForm(ags.getGoalSchema().getType()), ags);
					DefaultMutableTreeNode prolog2 = new DefaultMutableTreeNode(ags.getGoalSchema().getPrologDescription());
					goal.add(prolog2);
					DefaultMutableTreeNode suggested = new DefaultMutableTreeNode("Suggested: " + ags.getSuggested());
					goal.add(suggested);
					just_goals.add(goal);
				}
			}
		}
		
		DefaultMutableTreeNode actions = new DefaultMutableTreeNode("Current activities");
		for (Iterator<Operator> it = ExecutionState.getInstance().performingOperatorIterator(); it.hasNext(); ) {
			Operator currOperator = it.next();
			DefaultMutableTreeNode curr;
			if (currOperator instanceof StoryAction) {
				curr = new ActionNode(pm.shortForm(currOperator.getType()));
			} else if (currOperator instanceof FramingOperator) {
				curr = new FramingNode(pm.shortForm(currOperator.getType()));
			} else if (currOperator instanceof StoryEvent) {
				curr = new EventNode(pm.shortForm(currOperator.getType()));
			} else {
				curr = new DefaultMutableTreeNode("?: " + pm.shortForm(currOperator.getType()));
			}
			
			actions.add(curr);
		}
		if (actions.getChildCount() == 0) {
			actions.add(new DefaultMutableTreeNode("No current activities."));
		}
		
		DefaultMutableTreeNode action_history = new DefaultMutableTreeNode("Past activities");
		for (Iterator<Operator> it = ExecutionState.getInstance().executionHistoryIterator(); it.hasNext(); ) {
			Operator currOperator = it.next();
			DefaultMutableTreeNode curr;
			if (currOperator instanceof StoryAction) {
				curr = new ActionNode(pm.shortForm(currOperator.getType()));
			} else if (currOperator instanceof FramingOperator) {
				curr = new FramingNode(pm.shortForm(currOperator.getType()));
			} else if (currOperator instanceof StoryEvent) {
				curr = new EventNode(pm.shortForm(currOperator.getType()));
			} else {
				curr = new DefaultMutableTreeNode("?: " + pm.shortForm(currOperator.getType()));
			}
			
			action_history.add(curr);
		}
		if (action_history.getChildCount() == 0) {
			action_history.add(new DefaultMutableTreeNode("No past activities."));
		}		
		root.add(role);
		root.add(goals);
		root.add(just_goals);
		root.add(actions);
		root.add(action_history);
	
		//mindTree.expandPath(new TreePath(root));
		
		popup = new JPopupMenu();
	    popup.setOpaque(true);
	    popup.setLightWeightPopupEnabled(true);

	    mindTree.addMouseListener (
	        new MouseAdapter () {
	        	@Override
				public void mousePressed( MouseEvent e) {
	        		// UGLY! But causes the treenode to be selected upon right click as well.
	        		for (MouseListener ml: mindTree.getMouseListeners()) {
	        			if (ml != this) {
	        				// Simulate a left-click as well.
	        				MouseEvent me = new MouseEvent(
	        						mindTree, e.getID(), 
	        						System.currentTimeMillis(), 
	        						InputEvent.BUTTON1_MASK, 
	        						e.getX(), e.getY(), 
	        						1, 
	        						false);
	        				ml.mousePressed(me);

	        			}
	        		}
	        		
	        		//Discover the row that was selected (IE: The branch)
	        		int selRow = mindTree.getRowForLocation(e.getX(), e.getY());
	        		//Get the selection path for the row
	        		TreePath selPath = mindTree.getPathForLocation(e.getX(), e.getY());
	        		if(selRow != -1) {
		        		if(e.getClickCount() == 1) {
			        		if( e.isPopupTrigger()){
			        		//Make the selection
			        			mindTree.setSelectionPath(selPath);
			        		}
		        		}
	        		}
	        	}
	        		
	           @Override
			public void mouseReleased( MouseEvent e ) {
	              if ( e.isPopupTrigger()) {
	            	  popup.removeAll();
	      		    TreePath path = mindTree.getSelectionPath();
	      		    if (path != null) {
		    		    Object o = path.getLastPathComponent();
		    		    
		    		    if (o instanceof GoalNode) {
		          		  popup.add(new ShowPlanGraphAction("Show plan graph", null, ((GoalNode)o).getAdoptedGoalSchema()));
		          		  popup.add(new RePlanAction("Make new plan", null, ((GoalNode)o).getAdoptedGoalSchema()));
		    		    }
		    		    if (o instanceof JustifiableGoalNode) {
		    		    	popup.add(new ShowJustifyingPlanGraphAction("Show justifying plan graph", null, ((JustifiableGoalNode)o).getJustifiableGoalSchema()));
		    		    }
		          		
		    		    popup.show( (JComponent)e.getSource(), 
		                                 e.getX(), e.getY() );
	      		    }
	              } 
	           }
	        }
	    );
		
		expandAll(mindTree, true);	
		return mindTree;
	}
	
	// If expand is true, expands all nodes in the tree.
	// Otherwise, collapses all nodes in the tree.
	private void expandAll(JTree tree, boolean expand) {
		TreeNode root = (TreeNode) tree.getModel().getRoot();

		// Traverse tree from root
		expandAll(tree, new TreePath(root), expand);
	}
	
	private void expandAll(JTree tree, TreePath parent, boolean expand) {
		// Traverse children
		TreeNode node = (TreeNode) parent.getLastPathComponent();
		if (node.getChildCount() >= 0) {
			for (Enumeration e = node.children(); e.hasMoreElements();) {
				TreeNode n = (TreeNode) e.nextElement();
				if (! (
						n instanceof GoalNode ||
						n instanceof JustifiableGoalNode ||
						n instanceof ActionNode ||
						n instanceof EventNode ||
						n instanceof FramingNode)) {
					TreePath path = parent.pathByAddingChild(n);
					expandAll(tree, path, expand);
				}
			}
		}

		// Expansion or collapse must be done bottom-up
		if (expand) {
			tree.expandPath(parent);
		} else {
			tree.collapsePath(parent);
		}
	}	
	
	
	
	@Override
	public void update() {
		mindTree = createMindTree();		
		setContent(new JScrollPane(mindTree));		
	}
	
}
