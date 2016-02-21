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

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Paint;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Stroke;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPanel;

import org.apache.commons.collections15.Transformer;

import vs.debug.LogFactory;
import vs.poplanner.PlanLink;
import vs.poplanner.PlanOrdering;
import vs.poplanner.PlanStep;
import vs.poplanner.PoPlanner;
import edu.uci.ics.jung.algorithms.layout.FRLayout;
import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.graph.DirectedSparseMultigraph;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.util.EdgeType;
import edu.uci.ics.jung.visualization.DefaultVisualizationModel;
import edu.uci.ics.jung.visualization.GraphZoomScrollPane;
import edu.uci.ics.jung.visualization.VisualizationModel;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;
import edu.uci.ics.jung.visualization.control.ModalGraphMouse;
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller;
import edu.uci.ics.jung.visualization.picking.PickedInfo;
import edu.uci.ics.jung.visualization.renderers.Renderer;
import edu.uci.ics.jung.visualization.renderers.VertexLabelAsShapeRenderer;

public class PlanGraphPanel extends JPanel {
	
	public class EdgeTypeLabeller implements Transformer<PlanEdge,String> {
		public String transform(PlanEdge e) {
			return e.getType();
		}
	}
	public class PlanVertexPaintTransformer implements Transformer<PlanVertex,Paint> {
		protected PickedInfo<PlanVertex> m_pi;
		
		public PlanVertexPaintTransformer(PickedInfo<PlanVertex> pi) {
			m_pi = pi;
		}
		
		public Paint transform(PlanVertex e) {
			if (m_pi.isPicked(e)) {
				return Color.yellow;
			} else {
				if (e.getType() == null) {
					return Color.white;
				}
				if (e.getType().equals(FINISH) || e.getType().equals(START)) {
					return new Color(255,69,0);  // OrangeRed
				}			
				if (e.getType().equals(IMPROVISATION)) {
					return Color.ORANGE;
				}					
				if (e.getType().equals(ACTION)) {
					return new Color(176,196,222); // LightSteelBlue					

				} 
				if (e.getType().equals(EVENT)) {
					return Color.GREEN; // LightSteelBlue	
				}
				if (e.getType().equals(EXPECTATION)) {
					return new Color(225,235,255); // LightSteelBlue								

				} else {
					return Color.white;
				}
			}
		}
	}
	public class PlanVertexStrokeTransformer implements Transformer<PlanVertex,Stroke> {
		protected PickedInfo<PlanVertex> m_pi;
		
		public PlanVertexStrokeTransformer(PickedInfo<PlanVertex> pi) {
			m_pi = pi;
		}
		
		public Stroke transform(PlanVertex e) {
			if (m_pi.isPicked(e)) {
				return new BasicStroke(2);
			} else {
				if (e.getType() == null) {
					return new BasicStroke(1);
				}
				if (e.getType().equals(EXPECTATION)) {
					return new BasicStroke(1.0f,                      // Width
	                           BasicStroke.CAP_SQUARE,    // End cap
	                           BasicStroke.JOIN_MITER,    // Join style
	                           10.0f,                     // Miter limit
	                           new float[] {3.0f,3.0f}, // Dash pattern
	                           0.0f);                     // Dash phase
				} else {
					return new BasicStroke(1);
				}
			}
		}
	}
	public class TypedEdgePaintTransformer implements Transformer<PlanEdge,Paint> {
		protected PickedInfo<PlanEdge> m_pi;
		
		public TypedEdgePaintTransformer(PickedInfo<PlanEdge> pi) {
			m_pi = pi;
		}
		
		public Paint transform(PlanEdge e) {
			if (m_pi.isPicked(e)) {
				return Color.blue;
			} else {
				if (e.getType().equals("ordering")) {
					return Color.LIGHT_GRAY;
				}				
				if (e.getType().equals("link")) {
					return Color.DARK_GRAY;
				} else {
					return Color.black;
				}
			}
		}
	}
	public class VertexLabelAsShapeRendererWithPadding<PlanVertex,PlanEdge> extends VertexLabelAsShapeRenderer<PlanVertex,PlanEdge> {
		@Override
		public Shape transform(PlanVertex v) {
			Shape s = super.transform(v);			
			Shape t = new Rectangle(s.getBounds().x - 5, s.getBounds().y - 1, s.getBounds().width + 10, s.getBounds().height + 2);
			
			return t;
		}
	}
	protected static String START = "start";
	
	protected static String FINISH = "finished";
	
	protected static String ACTION = "action";
	
	protected static String EVENT = "event";
	protected static String IMPROVISATION = "framing";
	protected static String EXPECTATION = "expectation";
	protected static String LINK = "link";
	protected static String ORDERING = "ordering";
	private PoPlanner m_planner;
	
	private Graph<PlanVertex,PlanEdge> m_graph;
	private Layout m_layout;
	
	private Renderer m_renderer;
	
	private VisualizationViewer m_viewer;
	
	private JMenuBar m_menuBar;
	
	private GraphZoomScrollPane m_scrollPane;

	private Logger logger;
	
	protected Map<String,PlanVertex> vertices = new HashMap<String,PlanVertex>();
	
	public PlanGraphPanel(PoPlanner planner) {
		
		logger = LogFactory.getLogger(this);
		m_planner = planner;
        
		Dimension preferredSize = new Dimension(800,600);
		
		m_graph = new DirectedSparseMultigraph<PlanVertex,PlanEdge>();
		m_graph.addVertex(new PlanVertex("No plans.", null, null));
		
		VertexLabelAsShapeRenderer<PlanVertex,PlanEdge> vlasr = new VertexLabelAsShapeRendererWithPadding<PlanVertex,PlanEdge>();
		
		m_layout = new FRLayout<PlanVertex,PlanEdge>( m_graph );
		
		final VisualizationModel<PlanVertex,PlanEdge> visualizationModel = 
            new DefaultVisualizationModel<PlanVertex,PlanEdge>(m_layout, preferredSize);		
		
		m_viewer = new VisualizationViewer<PlanVertex,PlanEdge>(visualizationModel, preferredSize);
		m_viewer.setBackground(Color.white);
		m_viewer.getRenderContext().setVertexLabelTransformer(new ToStringLabeller());
		m_viewer.getRenderContext().setEdgeLabelTransformer(new EdgeTypeLabeller());
		//m_viewer.getRenderContext().setVertexLabelRenderer(new DefaultVertexLabelRenderer(Color.blue));
		m_viewer.getRenderContext().setVertexShapeTransformer(vlasr);
		//m_viewer.getRenderer().setVertexRenderer(new GradientVertexRenderer<PlanVertex,PlanEdge>(Color.LIGHT_GRAY, Color.white, true));
		m_viewer.getRenderContext().setVertexFillPaintTransformer(new PlanVertexPaintTransformer(m_viewer.getPickedVertexState()));
		m_viewer.getRenderContext().setEdgeDrawPaintTransformer(new TypedEdgePaintTransformer(m_viewer.getPickedEdgeState()));		
		m_viewer.getRenderContext().setVertexStrokeTransformer(new PlanVertexStrokeTransformer(m_viewer.getPickedVertexState()));	
		
		m_viewer.getRenderer().setVertexLabelRenderer(vlasr);
		m_viewer.getRenderer().getVertexLabelRenderer().setPosition(Renderer.VertexLabel.Position.CNTR);
		m_viewer.setEdgeToolTipTransformer(new ToStringLabeller());
		
		
		DefaultModalGraphMouse gm = new DefaultModalGraphMouse();
		gm.setMode(ModalGraphMouse.Mode.PICKING);
		m_viewer.setGraphMouse(gm);
		
		m_menuBar = new JMenuBar();
		JMenu modeMenu = gm.getModeMenu(); // Obtain mode menu from the mouse
		modeMenu.setText("Mouse Mode");
		modeMenu.setIcon(null); // I'm using this in a main menu
		modeMenu.setPreferredSize(new Dimension(80,20)); // Change the size
		m_menuBar.add(modeMenu);

		// create a frome to hold the graph
        m_scrollPane = new GraphZoomScrollPane(m_viewer);
        this.setLayout(new BorderLayout());
        add(m_scrollPane, BorderLayout.CENTER);
		add(m_menuBar, BorderLayout.NORTH);
		m_viewer.repaint();
		

	}	
	
	public Graph makeGraph() {
		// create a JGraphT graph
		DirectedSparseMultigraph g = new DirectedSparseMultigraph();
		Iterator<String> i;
		String n;
		//PoPlanner planner = m_ownerAgent.getPlanner();
		if (m_planner == null) {
			return null;
		}
		
		// STEPS
		
		for (PlanStep step: m_planner.getSteps()) {
				
			String name = step.getName();
			String operator = step.getOperator();
			String label = step.getOperatorType();
			String clss = step.getOperatorClass();
			
			//logger.fine("NAME: " + name + "\nSCHEMA: " + operator + "\nLABEL: " + label + "\nCLASS: " + clss + "\nAGENS: " + PrologKB.getInstance().getSchemaAgens(operator));
			if (label.equals(FINISH)) {
				clss = FINISH;
			}
			PlanVertex v1 = new PlanVertex(name, clss, label);
			g.addVertex(v1);
			vertices.put(name,v1);
		}

		PlanVertex start = new PlanVertex("s", START, "Start");
		g.addVertex(start);
		vertices.put("s", start);

		// ORDERINGS
		//i = m_planner.getOrderings().iterator();
		// TODO: make getOrderings() return a set.
		Set<Object> alreadyHad = new HashSet<Object>();
		for (PlanOrdering o: m_planner.getOrderings()) {
			if (! alreadyHad.contains(o)) {
				alreadyHad.add(o);
			
/*				String v1 = PrologKB.getInstance().first(n);
				String v2 = PrologKB.getInstance().second(n);
				
				String v1_name = PrologKB.getInstance().first(v1);
				String v2_name = PrologKB.getInstance().first(v2);
*/				
				PlanVertex left = vertices.get(o.getSrc());
				if (left == null) {
					left = new PlanVertex(o.getSrc(), "unknown", null);
				}
				PlanVertex right = vertices.get(o.getTarget());
				if (right == null) {
					right = new PlanVertex(o.getTarget(), "unknown", null);
				}				
				
				g.addEdge(new PlanEdge("ordering", "ordering"), left, right, EdgeType.DIRECTED);
			}
		}
		
		
		// LINKS
		alreadyHad.clear();
		for (PlanLink l: m_planner.getLinks()) {
		
			if (! alreadyHad.contains(l)) {
				alreadyHad.add(l);
				
/*				String from = PrologKB.getInstance().getPrologSingleVariable(PrologKB.planLinkFrom, n);
				String to = PrologKB.getInstance().getPrologSingleVariable(PrologKB.planLinkTo, n);
				String pos = PrologKB.getInstance().getPrologSingleVariable(PrologKB.planLinkPos, n);
				String neg = PrologKB.getInstance().getPrologSingleVariable(PrologKB.planLinkNeg, n);

				RDFtriple posTriple = PrologKB.getInstance().prologToTriple(pos, true);
				RDFtriple negTriple = PrologKB.getInstance().prologToTriple(neg, false);
*/				
				PlanVertex left = vertices.get(l.getSrc());
				if (left == null) {
					left = new PlanVertex(l.getSrc(), "unknown", null);
				}
				PlanVertex right = vertices.get(l.getTarget());
				if (right == null) {
					right = new PlanVertex(l.getTarget(), "unknown", null);
				}				
				
				g.addEdge(new PlanEdge(l.getCondition().toString(), "link"), left, right, EdgeType.DIRECTED);
			}
		}		

		return g;
	}
	
	public void refresh() {
		setVisible(false);
		m_layout.setGraph(makeGraph());
		m_viewer.repaint();
		setVisible(true);

	}

}
