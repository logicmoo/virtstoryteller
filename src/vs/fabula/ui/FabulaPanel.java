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
package vs.fabula.ui;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Paint;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.SystemColor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.MouseMotionListener;
import java.awt.geom.Point2D;
import java.util.Collection;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Set;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.Popup;
import javax.swing.PopupFactory;

import org.apache.commons.collections15.Predicate;
import org.apache.commons.collections15.Transformer;

import vs.Config;
import vs.StoryDomain;
import vs.debug.LogFactory;
import vs.fabula.FabulaKnowledgeBase;
import vs.fabula.IFabulaBuilder;
import vs.fabula.NamedGraphFabulaKnowledgeBase;
import vs.knowledge.vocab.Fabula;
import vs.rationalagent.StoryTime;
import vs.rationalagent.ui.UpdateablePanel;

import com.hp.hpl.jena.graph.Node;
import com.hp.hpl.jena.graph.Triple;
import com.hp.hpl.jena.ontology.Individual;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.rdf.model.Literal;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.reasoner.Reasoner;
import com.hp.hpl.jena.reasoner.ReasonerRegistry;
import com.hp.hpl.jena.util.iterator.ExtendedIterator;

import de.fuberlin.wiwiss.ng4j.NamedGraph;
import de.fuberlin.wiwiss.ng4j.NamedGraphSet;
import edu.uci.ics.jung.algorithms.layout.FRLayout;
import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.graph.DirectedSparseMultigraph;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.visualization.DefaultVertexLabelRenderer;
import edu.uci.ics.jung.visualization.GraphMouseListener;
import edu.uci.ics.jung.visualization.GraphZoomScrollPane;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;
import edu.uci.ics.jung.visualization.control.ModalGraphMouse;
import edu.uci.ics.jung.visualization.decorators.EllipseVertexShapeTransformer;
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller;
import edu.uci.ics.jung.visualization.picking.PickedInfo;
import edu.uci.ics.jung.visualization.renderers.Renderer;
import edu.uci.ics.jung.visualization.renderers.VertexLabelAsShapeRenderer;
import edu.uci.ics.jung.visualization.subLayout.GraphCollapser;
import edu.uci.ics.jung.visualization.util.PredicatedParallelEdgeIndexFunction;

/**
 * A panel that displays the fabula so far
 * 
 * @author swartjes
 *
 */
public class FabulaPanel extends UpdateablePanel implements GraphMouseListener {

	protected OntModel _reasoningModel;
	protected NamedGraphSet _fabulaSet;
	protected StoryDomain _storyDomain;
	private OntModelSpec reasoningSpec;
	private Reasoner reasoner;
	private Graph<FabulaVertex,FabulaEdge> m_graph;
	
	private Layout m_layout;
	
	private Renderer m_renderer;
	
	private VisualizationViewer m_viewer;
	
	private JMenuBar m_menuBar;
	
	private GraphZoomScrollPane m_scrollPane;
	GraphCollapser collapser;
	
	protected Logger logger;
	
	public FabulaPanel(IFabulaBuilder builder, StoryDomain sd) {

		logger = LogFactory.getLogger(this);
		_storyDomain = sd;
		
		reasoner = ReasonerRegistry.getRDFSReasoner();
		
		reasoningSpec = new OntModelSpec( OntModelSpec.OWL_MEM );
		reasoningSpec.setReasoner(reasoner);
		
		_reasoningModel = ModelFactory.createOntologyModel( reasoningSpec );
		_reasoningModel.setNsPrefixes(Config.namespaceMap);
		_reasoningModel.getDocumentManager().addAltEntry(_storyDomain.getOntology(), "file:" + _storyDomain.getAltEntry());
		_reasoningModel.getDocumentManager().addAltEntry(stripHash(Config.namespaceMap.get(Config.SWC_PREFIX)), "file:" + Config.SWC_URI);
		_reasoningModel.getDocumentManager().addAltEntry(stripHash(Config.namespaceMap.get(Config.FABULA_PREFIX)), "file:" + Config.FABULA_URI);		
		_reasoningModel.read(_storyDomain.getOntology());
		
		_fabulaSet = null;
		for (FabulaKnowledgeBase fkb: builder.getAllFabulaKnowledgeBase()) {
			if (fkb instanceof NamedGraphFabulaKnowledgeBase) {
				_fabulaSet = ((NamedGraphFabulaKnowledgeBase)fkb).getNamedGraphSet();
			}
		}
		
		if (_fabulaSet == null) {
			logger.severe("No named graph fabula knowledge base found!");
			return;
		}
		
		logger = LogFactory.getLogger(this);
		
		m_graph = new DirectedSparseMultigraph();
		VertexLabelAsShapeRenderer vlasr = new VertexLabelAsShapeRendererWithPadding(); //WithPadding<PlanVertex,PlanEdge>();

        collapser = new GraphCollapser(m_graph);
		
		m_layout = new MyFRLayout( m_graph );
		Dimension preferredSize = new Dimension(700,500);
		
		m_viewer = new VisualizationViewer(m_layout);
		m_viewer.addGraphMouseListener(this);
		
		//final VisualizationModel<FabulaVertex,FabulaEdge> visualizationModel = 
        //    new DefaultVisualizationModel<FabulaVertex,FabulaEdge>(m_layout, preferredSize);		
		
		//m_viewer = new VisualizationViewer<FabulaVertex,FabulaEdge>(visualizationModel, preferredSize);
		m_viewer.setBackground(Color.white);
		m_viewer.getRenderContext().setVertexLabelTransformer(new FabulaVertexLabeller());
		m_viewer.getRenderContext().setEdgeLabelTransformer(new EdgeNameLabeller());
		m_viewer.getRenderContext().setVertexLabelRenderer(new DefaultVertexLabelRenderer(Color.blue));
		m_viewer.getRenderContext().setVertexFillPaintTransformer(new FabulaVertexPaintTransformer(m_viewer.getPickedVertexState()));
		m_viewer.getRenderContext().setEdgeDrawPaintTransformer(new FabulaEdgePaintTransformer(m_viewer.getPickedEdgeState()));
		//m_viewer.getRenderContext().getEdgeLabelRenderer().setRotateEdgeLabels(false);
		m_viewer.getRenderContext().setVertexShapeTransformer(vlasr);

		//m_viewer.getRenderer().setVertexRenderer(new GradientVertexRenderer<PlanVertex,PlanEdge>(Color.LIGHT_GRAY, Color.white, true));
		//m_viewer.getRenderContext().setVertexFillPaintTransformer(new PlanVertexPaintTransformer(m_viewer.getPickedVertexState()));
		//m_viewer.getRenderContext().setEdgeDrawPaintTransformer(new TypedEdgePaintTransformer(m_viewer.getPickedEdgeState()));		
		m_viewer.getRenderContext().setVertexStrokeTransformer(new FabulaVertexStrokeTransformer(m_viewer.getPickedVertexState()));
		m_viewer.getRenderContext().setEdgeStrokeTransformer(new FabulaEdgeStrokeTransformer(m_viewer.getPickedEdgeState()));
		
		m_viewer.getRenderer().setVertexLabelRenderer(vlasr);
		m_viewer.getRenderer().getVertexLabelRenderer().setPosition(Renderer.VertexLabel.Position.CNTR);
		m_viewer.setEdgeToolTipTransformer(new ToStringLabeller());		
		
        final PredicatedParallelEdgeIndexFunction eif = PredicatedParallelEdgeIndexFunction.getInstance();
        final Set exclusions = new HashSet();
        eif.setPredicate(new Predicate() {

			public boolean evaluate(Object e) {
				
				return exclusions.contains(e);
			}});
        
        
        m_viewer.getRenderContext().setParallelEdgeIndexFunction(eif);
		
		DefaultModalGraphMouse gm = new DefaultModalGraphMouse();
		m_viewer.setGraphMouse(gm);
		gm.setMode(ModalGraphMouse.Mode.PICKING);
		
		m_menuBar = new JMenuBar();
		JMenu modeMenu = gm.getModeMenu(); // Obtain mode menu from the mouse
		modeMenu.setText("Mouse Mode");
		modeMenu.setIcon(null); // I'm using this in a main menu
		modeMenu.setPreferredSize(new Dimension(80,20)); // Change the size
		m_menuBar.add(modeMenu);
		
        JButton collapse = new JButton("Collapse");
        collapse.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                Collection picked = new HashSet(m_viewer.getPickedVertexState().getPicked());
                if(picked.size() > 1) {
                    Graph inGraph = m_layout.getGraph();
                    Graph clusterGraph = collapser.getClusterGraph(inGraph, picked);

                    Graph g = collapser.collapse(m_layout.getGraph(), clusterGraph);
                    double sumx = 0;
                    double sumy = 0;
                    for(Object v : picked) {
                    	Point2D p = (Point2D)m_layout.transform(v);
                    	sumx += p.getX();
                    	sumy += p.getY();
                    }
                    Point2D cp = new Point2D.Double(sumx/picked.size(), sumy/picked.size());
                    m_viewer.getRenderContext().getParallelEdgeIndexFunction().reset();
                    m_layout.setGraph(g);
                    m_layout.setLocation(clusterGraph, cp);
                    m_viewer.getPickedVertexState().clear();
                    //m_graph = g; // buggy hack
                    m_viewer.repaint();
                }
            }});
        
       
        JButton expand = new JButton("Expand");
        expand.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                Collection picked = new HashSet(m_viewer.getPickedVertexState().getPicked());
                for(Object v : picked) {
                    if(v instanceof Graph) {
                        
                        Graph g = collapser.expand(m_layout.getGraph(), (Graph)v);
                        m_viewer.getRenderContext().getParallelEdgeIndexFunction().reset();
                        m_layout.setGraph(g);
                    }
                    m_viewer.getPickedVertexState().clear();
                    m_viewer.repaint();
                }
            }});        
        
        m_menuBar.add(collapse);
        m_menuBar.add(expand);
		
		// create a frome to hold the graph
        m_scrollPane = new GraphZoomScrollPane(m_viewer);
        
        this.setLayout(new BorderLayout());
        add(m_scrollPane, BorderLayout.CENTER);
		add(m_menuBar, BorderLayout.NORTH);
		m_viewer.repaint();

	}
	
	/**
	 * Determines the kind of fabula element of a given Individual. Is it a Goal? Event? Action? etc.
	 * @param i the Jena Individual of the fabula element
	 * @return what kind of fabula it is.
	 */
	protected FabulaVertex.FabulaKind determineKind(Individual i) {
		
		Resource setting = _reasoningModel.getResource(Fabula.SettingElement);
		Resource event = _reasoningModel.getResource(Fabula.Event);
		Resource perception = _reasoningModel.getResource(Fabula.Perception);
		Resource internal_element = _reasoningModel.getResource(Fabula.InternalElement);
		Resource goal = _reasoningModel.getResource(Fabula.Goal);
		Resource action = _reasoningModel.getResource(Fabula.Action);
		Resource outcome = _reasoningModel.getResource(Fabula.Outcome);
		for (ExtendedIterator it = i.listRDFTypes(false); it.hasNext(); ) {
			Resource r = (Resource) it.next();
			if ( r.equals(setting)) {
				return FabulaVertex.FabulaKind.Setting;
			}
			if ( r.equals(event)) {
				return FabulaVertex.FabulaKind.Event;
			}
			if ( r.equals(perception)) {
				return FabulaVertex.FabulaKind.Perception;
			}
			if ( r.equals(internal_element)) {
				return FabulaVertex.FabulaKind.InternalElement;
			}			
			if ( r.equals(goal)) {
				return FabulaVertex.FabulaKind.Goal;
			}
			if ( r.equals(action)) {
				return FabulaVertex.FabulaKind.Action;
			}
			if ( r.equals(outcome)) {
				return FabulaVertex.FabulaKind.Outcome;
			}
		}
		return FabulaVertex.FabulaKind.FabulaElement;
	}
	
	/**
	 * Returns the fabula vertex for given RDF node.
	 * 
	 * @param n an RDF node 
	 * @return the fabula vertex in the graph, if it exists.
	 */
	protected FabulaVertex getVertexForNode(RDFNode n) {
		Individual i =(Individual) n.as(Individual.class);
		Resource cls = i.getRDFType(true);
		//logger.info("Fabula element Individual: " + i + " of class: " + cls );
		FabulaVertex fv = FabulaVertex.getVertex(i, cls.getLocalName(), determineKind(i));
		return fv;
	}
	
	/**
	 * Returns a string representing the information the user gets to see when double clicking on the node.
	 * @param fv the Fabula vertex
	 * @return a String containing (HTML) info.
	 */
	public String getVertexInfo(FabulaVertex fv) {
		com.hp.hpl.jena.shared.PrefixMapping pm = com.hp.hpl.jena.shared.PrefixMapping.Factory.create();
		pm.setNsPrefixes(vs.Config.namespaceMap);

		RDFNode timeNode = fv.getIndividual().getPropertyValue(_reasoningModel.getProperty(Fabula.time));
		Literal timeLit = (Literal) timeNode.as(Literal.class);
		Object time = timeLit.getValue();
		
		RDFNode characterNode = fv.getIndividual().getPropertyValue(_reasoningModel.getProperty(Fabula.character));
		RDFNode agens = fv.getIndividual().getPropertyValue(_reasoningModel.getProperty(Fabula.agens));
		RDFNode patiens = fv.getIndividual().getPropertyValue(_reasoningModel.getProperty(Fabula.patiens));
		RDFNode target = fv.getIndividual().getPropertyValue(_reasoningModel.getProperty(Fabula.target));
		RDFNode instrument = fv.getIndividual().getPropertyValue(_reasoningModel.getProperty(Fabula.instrument));
		
		RDFNode content = fv.getIndividual().getPropertyValue(_reasoningModel.getProperty(Fabula.hasContent));
		
		StringBuilder sb = new StringBuilder();
		if (content != null) {
			Individual contentInd = ( (Individual) content.as(Individual.class)); 
			boolean truth_true = contentInd.hasRDFType(_reasoningModel.getProperty(Fabula.TruthGraph));

			NamedGraph sub = _fabulaSet.getGraph(content.asNode());

			for (ExtendedIterator ei = sub.find(Node.ANY, Node.ANY, Node.ANY); ei.hasNext(); ) {
				Triple t = (Triple) ei.next();
				if (! truth_true) { sb.append("<b>not </b>"); }
				sb.append(pm.shortForm(t.getSubject().toString())).append(" ")
					.append(pm.shortForm(t.getPredicate().toString())).append(" ")
					.append(pm.shortForm(t.getObject().toString())).append(" ")
					.append("<BR>");
			}
			
		}
		
		StringBuilder popTxt = new StringBuilder();
		popTxt.append("<HTML>");
		popTxt.append("<B>").append(pm.shortForm(fv.getIndividual().toString())).append("</B> ");
		popTxt.append("is a " + fv.getKind().toString());
		popTxt.append("<TABLE cellspacing=0 cellpadding=4>");		   
		popTxt.append("<TR><TD align=right>").append("RDF type:").append("</TD><TD>").append(pm.shortForm(fv.getRDFType())).append("</TD></TR>");
		popTxt.append("<TR><TD align=right>").append("character:").append("</TD><TD>").append(pm.shortForm(characterNode.toString())).append("</TD></TR>");
		if (agens != null) {
			popTxt.append("<TR><TD align=right>").append("agens:").append("</TD><TD>").append(pm.shortForm(agens.toString())).append("</TD></TR>");	
		}
		if (patiens != null) {
			popTxt.append("<TR><TD align=right>").append("patiens:").append("</TD><TD>").append(pm.shortForm(patiens.toString())).append("</TD></TR>");	
		}
		if (target != null) {
			popTxt.append("<TR><TD align=right>").append("target:").append("</TD><TD>").append(pm.shortForm(target.toString())).append("</TD></TR>");	
		}
		if (instrument != null) {
			popTxt.append("<TR><TD align=right>").append("instrument:").append("</TD><TD>").append(pm.shortForm(instrument.toString())).append("</TD></TR>");	
		}
		popTxt.append("<TR><TD align=right>").append("time:").append("</TD><TD>").append(pm.shortForm(time.toString())).append("</TD></TR>");
		if (content != null) {
			popTxt.append("<TR><TD align=right>").append("contents:").append("</TD><TD>").append(sb.toString()).append("</TD></TR>");
		}
		popTxt.append("</TABLE>");
		popTxt.append("<HR>");
		popTxt.append("<DIV align=right><small>click to close</small></DIV>");
		popTxt.append("</HTML>");
		

   
		return popTxt.toString();

	}
	
	public void graphClicked(Object o, MouseEvent e) {
		if (e.getClickCount() == 2) {

			// Double click
			if (o instanceof FabulaVertex) {
				FabulaVertex fv = (FabulaVertex) o;
				   PopupFactory factory = PopupFactory.getSharedInstance();
				   JEditorPane contents = new JEditorPane();
				   contents.setBorder(BorderFactory.createLineBorder(Color.black));
				   contents.setContentType("text/html");
				   contents.setEditable(false);
				   contents.setBackground(SystemColor.info);
				   
				   contents.setText(getVertexInfo(fv));

				   final Popup popup = factory.getPopup(this, contents, e.getXOnScreen(), e.getYOnScreen());
				   Component c = this;
				   contents.addMouseListener(new java.awt.event.MouseAdapter() {
		               @Override
					public void mouseClicked(java.awt.event.MouseEvent evt) {
		                   popup.hide();
		               }
				   });		   
				   
				   popup.show();

			}
		   //popup.hide();
		}
	}
	
	public void graphPressed(Object fv, MouseEvent e) {}

	public void graphReleased(Object fv, MouseEvent e) {}	
	
	protected Graph makeGraph(Graph baseGraph) {
		
		Graph<FabulaVertex,FabulaEdge> g = baseGraph;
		
		if (_fabulaSet == null) {
			return g;
		}
		
		_reasoningModel.add(_fabulaSet.asJenaModel(""));

		
		Resource rc = _reasoningModel.getResource(Fabula.FabulaElement);
		
		for (StmtIterator it = _reasoningModel.listStatements(); it.hasNext(); ) { 
			Statement s = it.nextStatement();
			if (s.getPredicate().equals(_reasoningModel.getProperty(Fabula.phi_causes))) {
				FabulaVertex src = getVertexForNode(s.getSubject());
				FabulaVertex tgt = getVertexForNode(s.getObject()); 
				g.addVertex(src);
				g.addVertex(tgt);

				g.addEdge(FabulaEdge.getEdge("phi", src, tgt), src, tgt);
			}
			if (s.getPredicate().equals(_reasoningModel.getProperty(Fabula.psi_causes))) {
				FabulaVertex src = getVertexForNode(s.getSubject());
				FabulaVertex tgt = getVertexForNode(s.getObject()); 
				
				// Filter out dead ends
			
				g.addVertex(src);
				g.addVertex(tgt);
				
				g.addEdge(FabulaEdge.getEdge("psi", src, tgt), src, tgt);
			}
			if (s.getPredicate().equals(_reasoningModel.getProperty(Fabula.motivates))) {
				FabulaVertex src = getVertexForNode(s.getSubject());
				FabulaVertex tgt = getVertexForNode(s.getObject()); 
				g.addVertex(src);
				g.addVertex(tgt);
				
				g.addEdge(FabulaEdge.getEdge("motivates", src, tgt), src, tgt);
			}	
			if (s.getPredicate().equals(_reasoningModel.getProperty(Fabula.enables))) {
				FabulaVertex src = getVertexForNode(s.getSubject());
				FabulaVertex tgt = getVertexForNode(s.getObject()); 
				g.addVertex(src);
				g.addVertex(tgt);
				
				g.addEdge(FabulaEdge.getEdge("enables", src, tgt), src, tgt);
			}
			if (s.getPredicate().equals(_reasoningModel.getProperty(Fabula.resolves))) {
				FabulaVertex src = getVertexForNode(s.getSubject());
				FabulaVertex tgt = getVertexForNode(s.getObject()); 
				g.addVertex(src);
				g.addVertex(tgt);

				g.addEdge(FabulaEdge.getEdge("resolves", src, tgt), src, tgt);
			}			
			
		}
		return g;

	}
	
	protected Graph filterGraph(Graph input) {
		Graph<FabulaVertex,FabulaEdge> g = input;
		HashSet remove = new HashSet();
		
		// Take uninteresting dead-ends out.
		for (FabulaVertex fv: g.getVertices()) {
			if ((fv.getKind().equals(FabulaVertex.FabulaKind.InternalElement) ||
					fv.getKind().equals(FabulaVertex.FabulaKind.Perception) ||
					fv.getKind().equals(FabulaVertex.FabulaKind.Setting) ) 
				&& g.getOutEdges(fv).size() == 0 ) {
				remove.add(fv);
				for (FabulaEdge fe: g.getIncidentEdges(fv)) {
					remove.add(fe);
				}
			}
		}
		
		if (remove.size() > 0) {
			for (Object o: remove) {
				if (o instanceof FabulaVertex) {
					g.removeVertex((FabulaVertex)o);
				}
				if (o instanceof FabulaEdge) {
					g.removeEdge((FabulaEdge)o);
				}
			}

			// Recurse as long as stuff to remove.
			return filterGraph(g);
		}
		return g;
		
	}

	
	/**
	 * Removes the hash at the end of given string, if it exists
	 * 
	 * @param orig the original string (with potential hash, #) 
	 * @return the string without the last end hash
	 */
	protected String stripHash(String orig) {
		if (orig.endsWith("#")) {
			return orig.substring(0, orig.length()-1);
		} else {
			return orig;
		}
	}

	/**
	 * Inner classes
	 */
	
	@Override
	public void update() {
		// TODO Auto-generated method stub
		setVisible(false);
		Graph g = makeGraph(m_graph);
		m_layout.setGraph(filterGraph(g));
		m_viewer.repaint();
		setVisible(true);
	}
	
	public class EdgeNameLabeller implements Transformer<Object,String> {
		public String transform(Object e) {
			if (e instanceof FabulaEdge) {
				
				return ((FabulaEdge)e).getName();
			} else {
				return "";
			}
		}
	}
	
	public class FabulaVertexLabeller implements Transformer<Object,String> {
		public String transform(Object e) {
			if (e instanceof FabulaVertex) {
				
				return ((FabulaVertex)e).toString();
			} else {
				return "Multiple";
			}
		}
	}
	
	
	public class FabulaVertexPaintTransformer implements Transformer<Object,Paint> {
		protected PickedInfo m_pi;
		
		public FabulaVertexPaintTransformer(PickedInfo pi) {
			m_pi = pi;
		}
		
		public Paint transform(Object o) {

			if (m_pi.isPicked(o)) {
				return Color.yellow;
			} else {
				if (o instanceof FabulaVertex) {
					FabulaVertex e = (FabulaVertex)o;				
					
					if (e.getKind() == FabulaVertex.FabulaKind.Goal) {
						return Color.red;
					}
					if (e.getKind() == FabulaVertex.FabulaKind.Action) {
						return new Color(176,196,222);
					}
					if (e.getKind() == FabulaVertex.FabulaKind.Event) {
						return Color.green;
					}
					if (e.getKind() == FabulaVertex.FabulaKind.Setting) {
						return Color.orange;
					}
					if (e.getKind() == FabulaVertex.FabulaKind.Outcome) {
						return Color.white;
					}
				}					
				return Color.lightGray;
			}
		}
	}
	
	public class FabulaEdgePaintTransformer implements Transformer<Object,Paint> {
		protected PickedInfo m_pi;
		
		public FabulaEdgePaintTransformer(PickedInfo pi) {
			m_pi = pi;
		}
		
		public Paint transform(Object o) {

			if (m_pi.isPicked(o)) {
				return Color.yellow;
			} else {
				if (o instanceof FabulaEdge) {
					FabulaEdge e = (FabulaEdge)o;				
					
					if (e.getName().equals("enables")) {
						return Color.LIGHT_GRAY;
					} 
				}					
				return Color.black;
			}
		}
	}	
	
	public class FabulaVertexStrokeTransformer implements Transformer<Object,Stroke> {
		protected PickedInfo m_pi;
		
		public FabulaVertexStrokeTransformer(PickedInfo pi) {
			m_pi = pi;
		}
		
		public Stroke transform(Object e) {
			if (m_pi.isPicked(e)) {
				return new BasicStroke(2);
			} else {
				// If an edge is picked, make the vertex bold.
				if (e instanceof FabulaVertex) {
					for (FabulaEdge fe: m_graph.getIncidentEdges((FabulaVertex) e)) {
						if (m_viewer.getPickedEdgeState().isPicked(fe)) {
							return new BasicStroke(2);
						}
					}
				}
				return new BasicStroke(1);
			}

		}
	}
	
	public class FabulaEdgeStrokeTransformer implements Transformer<Object,Stroke> {
		protected PickedInfo m_pi;
		
		public FabulaEdgeStrokeTransformer(PickedInfo pi) {
			m_pi = pi;
		}
		
		public Stroke transform(Object e) {
			if (m_pi.isPicked(e)) {
				return new BasicStroke(2);
			} else {
				// If an edge is picked, make the vertex bold.
				if (e instanceof FabulaEdge) {
					for (FabulaVertex fv: m_graph.getIncidentVertices((FabulaEdge) e)) {
						if (m_viewer.getPickedVertexState().isPicked(fv)) {
							return new BasicStroke(2);
						}
					}
				}
				return new BasicStroke(1);
			}

		}
	}	
	
	class MyFRLayout extends FRLayout {

		public MyFRLayout(Graph g) {
			super(g);
			
			this.setInitializer(new VertexPosInitializer());
		}
		
		@Override
		public double getX(Object v) {
			return 100*StoryTime.getTime();
		}
		
	}
	public class VertexLabelAsShapeRendererWithPadding extends VertexLabelAsShapeRenderer {
		
		ClusterVertexShapeFunction cv;
		
		public VertexLabelAsShapeRendererWithPadding() {
			cv = new ClusterVertexShapeFunction();
		}
		
		@Override
		public Shape transform(Object v) {
            if(v instanceof Graph) {
            	return cv.transform(v);
            } else {
            
				Shape s = super.transform(v);			
				Shape t = new Rectangle(s.getBounds().x - 5, s.getBounds().y - 1, s.getBounds().width + 10, s.getBounds().height + 2);
				return t;
            }
			
		}
	}
	
	class VertexPosInitializer implements Transformer {
		public Point2D transform(Object fv) {
			Point2D p = new Point2D.Double(100*StoryTime.getTime(),0);
			
			return p;
		}
	}
	
    /**
     * a demo class that will create a vertex shape that is either a
     * polygon or star. The number of sides corresponds to the number
     * of vertices that were collapsed into the vertex represented by
     * this shape.
     * 
     * @author Tom Nelson
     *
     * @param <V>
     */
    class ClusterVertexShapeFunction<V> extends EllipseVertexShapeTransformer<V> {

        ClusterVertexShapeFunction() {
            setSizeTransformer(new ClusterVertexSizeFunction<V>(20));
        }
        @Override
        public Shape transform(V v) {
            if(v instanceof Graph) {
                int size = ((Graph)v).getVertexCount();
                if (size < 8) {   
                    int sides = Math.max(size, 3);
                    return factory.getRegularPolygon(v, sides);
                }
                else {
                    return factory.getRegularStar(v, size);
                }
            }
            return super.transform(v);
        }
    }
    
    /**
     * A demo class that will make vertices larger if they represent
     * a collapsed collection of original vertices
     * @author Tom Nelson
     *
     * @param <V>
     */
    class ClusterVertexSizeFunction<V> implements Transformer<V,Integer> {
    	int size;
        public ClusterVertexSizeFunction(Integer size) {
            this.size = size;
        }

        public Integer transform(V v) {
            if(v instanceof Graph) {
                return 30;
            }
            return size;
        }
    }
	

}
