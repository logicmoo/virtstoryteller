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
package vs.plotagent.inspiration;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import vs.Config;
import vs.knowledge.vocab.Fabula;
import vs.utils.UniqueId;

import com.hp.hpl.jena.graph.Node;
import com.hp.hpl.jena.vocabulary.RDF;
import com.hp.hpl.jena.vocabulary.RDFS;

import de.fuberlin.wiwiss.ng4j.Quad;

public class InspirationRule {

	public static String mainGraphVar = "Vmain";

	// TODO: make protected
	public String m_name;
	public Set<Node> m_subgraphnodes;
	public Node m_inspirationNode;
	public Node m_inspirationType;
	public Set<Node> m_inspirationCausers;
	public Set<Quad> m_antecedent;
	public Set<Quad> m_consequent;
	public Map<Node, String> m_nodeToVarMap;
	public Set<String> m_uniqueVars;
	
	//public Set<String> m_maingraphVars;
	public String m_maingraphVar;
	
	public int m_varCounter;

	private InspirationRule(Node inspirationNode, 
							Node inspirationType, 
							Set<Node> inspirationCausers, 
							Set<Node> subGraphNodes) {
		m_antecedent = new HashSet<Quad>();
		m_consequent = new HashSet<Quad>();
		m_inspirationNode = inspirationNode;
		m_inspirationType = inspirationType;
		m_inspirationCausers = inspirationCausers;
		m_subgraphnodes = subGraphNodes;
		m_nodeToVarMap = new HashMap<Node, String>();
		m_uniqueVars = new HashSet<String>();
		//m_maingraphVars = new HashSet<String>();

	}

	public InspirationRule(String name, 
							Node inspirationNode, 
							Node inspirationType, 
							Set<Node> inspirationCausers, 
							Set<Node> subGraphNodes) {
		this(inspirationNode, inspirationType, inspirationCausers, subGraphNodes);

		m_name = name;
	}

	public void add(InspirationRule r) {
		m_antecedent.addAll(r.getAntecedent());
		m_consequent.addAll(r.getConsequent());
		m_inspirationCausers.addAll(r.getInspirationCausers());
		//m_nodeToVarMap.putAll(r.getVars());
	}
	
	/**
	 * Adds a quad to the antecedent (left hand side) of the rule. A rule fires when its antecedent is met.
	 * TODO filter out stuff that a rule is not interested in? (e.g., rdfs:label) 
	 * 
	 * @param q the quad to add to the antecedent of the rule. 
	 */
	public void addAntecedent(Quad q) {
		if (filterAccepts(q)) {
			m_antecedent.add(q);
		}
	}
	
	/**
	 * Adds a quad to the consequent (right hand side) of the rule. When a rule fires, it produces the consequent.
	 * @param q the quad to add to the consequent of the rule.
	 */
	public void addConsequent(Quad q) {
		if (filterAccepts(q)) {
			m_consequent.add(q);
		}
	}

	private String antecedentToString(Set<Quad> quadSet) {

		StringBuilder sb = new StringBuilder();

		boolean firstTime = true;

		for (Quad q : quadSet) {
			if (!firstTime) {
				sb.append(",\n");
			}

			sb.append(quadToString(q));

			firstTime = false;
		}

		return sb.toString();
	}	

	private String consequentToString(Set<Quad> quadSet) {

		StringBuilder sb = new StringBuilder();

		boolean firstTime = true;

		sb.append('[');

		for (Quad q : quadSet) {
			if (!firstTime) {
				sb.append(",\n");
			}
			sb.append("  ").append(quadToString(q, true));
			firstTime = false;
		}

		sb.append("\n]");

		return sb.toString();
	}

	/**
	 * Following the steps from http://java.sun.com/developer/Books/effectivejava/Chapter3.pdf
	 * It must be symmetric, transitive and consistent
	 */
	@Override
	public boolean equals(Object o) {
		// 1. check for equality
		if (this == o)
			return true;

		// 2. check for type
		if (!(o instanceof InspirationRule))
			return false;

		// 3. typecast
		InspirationRule ir = (InspirationRule) o;

		boolean equalCheck = true;

		// 4. Check for equality of important fields. 
		//    We are only interested in the antecedents, consequents, and inspiration-causers.
		return ir.getAntecedent().equals(this.getAntecedent())
				&& ir.getConsequent().equals(this.getConsequent())
				&& ir.getInspirationCausers().equals(this.getInspirationCausers());
	}

	/*public Map<Node,String> getVars() {
		return m_nodeToVarMap;
	}
	
	public void addVar(Node n, String var) {
		m_nodeToVarMap.put(n, var);
	}*/

	private boolean filterAccepts(Quad q) {
		boolean accept = true;

		if (q.getPredicate().matches(RDFS.label.asNode())) {
			accept = false;
		}
		// Ignore time for now; a rule should fire even if the exact times don't match!
		// TODO: time does matter, e.g. in determining the interval in which a rule can apply. 
		// If time is specified in the narrative case, then it is relevant; use it sensibly (relative). 
		if (q.getPredicate().matches(Node.createURI(Fabula.time))) {
			accept = false;
		}

		return accept;
	}

	public Set<Quad> getAntecedent() {
		return m_antecedent;
	}

	public Set<Quad> getConsequent() {
		return m_consequent;
	}

	public Set<Node> getInspirationCausers() {
		return m_inspirationCausers;
	}

	public Node getInspirationNode() {
		return m_inspirationNode;
	}

	public Node getInspirationType() {
		return m_inspirationType;
	}

	public String getName() {
		return m_name;
	}

	/** 
	 * Return the sum of the hashcodes of the important parts. hashCode must be overridden when equals() is used.
	 */
	@Override
	public int hashCode() {
		int result = 17; // arbitrary
		result = 37 * result + getAntecedent().hashCode(); // 37 is an odd prime
		result = 37 * result + getConsequent().hashCode();
		result = 37 * result + getInspirationCausers().hashCode();

		return result;
	}
	
	private String makeVar(Node elem, boolean isGraph) {
		// If we're making a variable out of a graph node, and it is the main graph, use dont-care _
		if (isGraph && (!m_subgraphnodes.contains(elem))) {
			return mainGraphVar;
		}
		return "V" + elem.getLocalName().replace('.', '_');
	}

	@Deprecated
	private String makeVarOld(Node elem, boolean isGraph) {

		// If we're making a variable out of a graph node, and it is the main graph, use dont-care _
		if (isGraph && (!m_subgraphnodes.contains(elem))) {
			return "_";
		}
		String var = (isGraph ? "_V" : "V");
		return var + elem.getLocalName().replace('.', '_');
	}
	
	private Map<Node, String> makeVars(Set<Quad> quadList) {

		//NamedGraphModel m = m_owner.getNamedGraph().asJenaModel("");
		
		// TODO: niet zomaar alle individuals vervangen door variables! Standaard niet doen,
		// eventuele casetransformers definieren variaties. Nu verlies je alle semantiek onder
		// het niveau van OWL classes.

		// Meer specifiek: als de Individuals al bestaan (in de ontology), gebruik ze, zo niet, maak ze variabel
		// in dit geval, moet de informatie over de Individual uit de ontology ook in de regel terechtkomen? nah. Zit al in de world knowledge. 

		for (Quad q : quadList) {

			// Test for RDF:Type
			if (q.getPredicate().matches(RDF.type.asNode())) {
				// add subject as variable (because it is an Individual)
				if (m_nodeToVarMap.get(q.getSubject()) == null) {
					m_nodeToVarMap.put(q.getSubject(), makeVar(q.getSubject(),
							false));
				}
			}

			// Test for graph names
			// assumption: graph names do not matter. Might be wrong; the main graph name might matter
			// otherwise the rule can match because a particular subgraph contains the required LHS
			// TODO: when graph names don't matter, Prolog will unify triples that do not exist! Graph names DO matter.
			if (m_nodeToVarMap.get(q.getGraphName()) == null) {
				String var = makeVar(q.getGraphName(), true);
				m_nodeToVarMap.put(q.getGraphName(), var);
				
				if (m_subgraphnodes.contains(q.getGraphName())) {
					// We're talking about a subgraph; make it unique (two different subgraph variables must not bind to the same named graph URI)
					m_uniqueVars.add(var);
					
				}
			}
			
		}

		return m_nodeToVarMap;
	}
	
	private String nodeToString(Node n) {
		return nodeToString(n, false);
	}
	
/*	private String maingraphVarsToString() {
		StringBuilder sb = new StringBuilder();
		boolean firsttime = true;
		
		for (String vm: m_maingraphVars) {
			for (String vs: m_uniqueVars) {
				// add rule that the variables must be bound to different values
				if (! firsttime) {
					sb.append(',');
				}
				sb.append(vm).append(" \\= ").append(vs);
				firsttime = false;
			}
		}
		
		return sb.toString();
	}*/

	private String nodeToString(Node n, boolean isConsequent) {
		StringBuilder sb = new StringBuilder();

		String var = m_nodeToVarMap.get(n);

		if (var != null) {
			if (isConsequent) {
				if (m_subgraphnodes.contains(n)) {
					// add to new subgraph
					sb.append(var);

				} else {
					if (m_maingraphVar != null) {
//					if (m_maingraphVars.contains(m_maingraphVar)) {
						sb.append(m_maingraphVar);						
					} else {
						sb.append("maingraph");
					}
				}
			} else {
				if (var.equals(mainGraphVar)) {
	
					// This means, we're talking about a maingraph named graph.
					// Prolog uses the source of a triple to denote a named graph. However, the source of triples
					// of the main graph come from ontologies (the setting), or assertions by the application (the changes)
					// therefore, each maingraph triple gets a different variable, to afford a rule in theory to match with
					// main graph knowledge from as many different sources as there are main graph triples. 
					// TODO: when we move to SWI-Prolog version 5.6.37 or higher, we can explicitly set the source of triples
					// (i.e., to "mainGraph"), and we can revert to using one variable again.
					
					m_varCounter++;
					String uniqueVar;
					if (m_maingraphVar == null) {
						uniqueVar = "Vmain_" + m_varCounter;
						m_maingraphVar = uniqueVar;
					} else {
						uniqueVar = "_Vmain_" + m_varCounter;
					}
					sb.append(uniqueVar);
									
		
				} else {
					sb.append(var);
				}
			}

		} else {
			if (m_subgraphnodes.contains(n)) {
				// make new subgraph node
				// TODO: is this unique enough?
				StringBuilder newGraphBuilder = new StringBuilder();  
				newGraphBuilder.append('\'').append(Config.namespaceMap.get("graph")).append(UniqueId.generateUniqueIndividual("subgraph", "plotagent")).append('\'');
				m_nodeToVarMap.put(n, newGraphBuilder.toString());
				sb.append(newGraphBuilder);
			} else {
				if (n.isLiteral()) {
					sb.append("literal(");
					sb.append("type(");
					sb.append('\'').append(n.getLiteralDatatypeURI()).append('\'')
							.append(',');
					sb.append('"').append(n.getLiteralValue()).append('"');
					sb.append(')');
					sb.append(')');
				} else {
					sb.append('\'').append(n).append('\'');
				}
			}
		}
		return sb.toString();
		
	}

	private String quadToString(Quad q) {
		return quadToString(q, false);
	}
	
	private String quadToString(Quad q, boolean isConsequent) {
		StringBuilder sb = new StringBuilder();
		sb.append("rdf(");
		sb.append(nodeToString(q.getSubject()));
		sb.append(',');
		sb.append(nodeToString(q.getPredicate()));
		sb.append(',');
		sb.append(nodeToString(q.getObject()));
		sb.append(',');
		sb.append(nodeToString(q.getGraphName(), isConsequent));
		sb.append(')');

		return sb.toString();
		
	}
	

	/**
	 * A rule looks like this:
	 * 
	 *     suggestion('rule_14', (<quada>, <quadb>, <...>)) 
	 *       :-
	 *       ( <quad1>, <quad2>, <...>)
	 */

	@Override
	public String toString() {
		// TODO: replace individuals by variables.
		// TODO: make version that compiles into operators
		makeVars(m_antecedent);
		//makeVars(m_consequent);

		StringBuilder sb = new StringBuilder();

		sb.append("suggestion(");

		sb.append('\'').append(getName()).append("', ");
		
		sb.append('\'').append(getInspirationNode()).append("', ");
		sb.append('\'').append(getInspirationType()).append("', ");
		
		sb.append('[');
		boolean firstTime = true;
		for (Node n: getInspirationCausers()) {
			if (! firstTime) {
				sb.append(',');
			}
			sb.append(nodeToString(n));
			firstTime = false;
		}
		sb.append("],\n");

		// first process antecedent (because antecedent determines variable bindings for consequent)!
		String antecedent = antecedentToString(getAntecedent());
		
		sb.append(consequentToString(getConsequent()));
		
		sb.append(")\n:- \n");

		sb.append(antecedent);
		
		if (m_uniqueVars.size() > 1) {
			sb.append(",\n");
		
			sb.append(uniqueVarsToString());
		}
		
/*		if (m_maingraphVars.size() > 0 && m_uniqueVars.size()>0 ) {
			sb.append(",\n");
			
			sb.append(maingraphVarsToString());
		}*/
		
		sb.append('.');

		return sb.toString();
	}

	/*
	 * Makes sure that the unique vars bind to different values by stating an inequality relationship between them.
	 */
	private String uniqueVarsToString() {
		StringBuilder sb = new StringBuilder();
		boolean firsttime = true;
		
		for (String v1: m_uniqueVars) {
			for (String v2: m_uniqueVars) {
				if (! v1.equals(v2)) {
					// add rule that the variables must be bound to different values
					if (! firsttime) {
						sb.append(',');
					}
					sb.append(v1).append(" \\= ").append(v2);
					firsttime = false;
				}
			}
		}
		
		return sb.toString();
	}

}
