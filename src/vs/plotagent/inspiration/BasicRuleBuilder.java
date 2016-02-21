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
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;

import vs.knowledge.vocab.Fabula;

import com.hp.hpl.jena.graph.Node;
import com.hp.hpl.jena.vocabulary.RDF;

import de.fuberlin.wiwiss.ng4j.NamedGraphSet;
import de.fuberlin.wiwiss.ng4j.Quad;

/**
 * Basic implementation of the RuleBuilder.<br><br>
 * 
 * It will look at the causal chains of the fabula and build rules of them, on the following principles:<BR><BR>
 * 
 * (1) if there is A and B, and A causes B, then you can make a rule:<br>
 * 			<code>IF A, THEN B /\ A causes B</code><br><br>
 * 
 * (2) if there is A1..An and B, and A1..An cause B, then you can make a rule:<br>
 * 			<code>IF A1 /\ A2 /\ .. /\ An, THEN B /\ A1 causes B /\ A2 causes B /\ ... /\ An causes B</code><br><br>
 * 
 * (3) if there is a causal chain leading up to A, and A causes B, then you can make a rule containing the
 *     whole causality information leading up to A:<br>
 *          <code>IF .. /\ .. /\ .. causes A, THEN B and A causes B</code><br><br>
 * 
 * @author swartjes
 *
 */
public class BasicRuleBuilder extends RuleBuilder {

	/** The URIs of the predicates considered to be causal links, according to the Fabula ontology. **/
	public static String[] causalLinks = { Fabula.motivates, Fabula.phi_causes,
			Fabula.psi_causes, Fabula.enables };

	/** To keep track which nodes are fabula nodes, used to for instance select the quads that don't pertain to fabula nodes **/
	private final Set<Node> m_fabulaNodes;

	/** For each fabula node, a (local) rule is stored. Fabula nodes further up in the chain will append to these rules **/
	private final Map<Node, InspirationRule> m_nodeRules;

	/** Contains all the nodes that represent a sub graph (i.e., all but the mainGraph graph)**/
	private final Set<Node> m_subGraphs;

	/**
	 * Constructor
	 * @param ngs the NamedGraphSet containing the source case from which to build rules.
	 */
	public BasicRuleBuilder(NamedGraphSet ngs) {
		super(ngs);

		m_fabulaNodes = new HashSet<Node>();
		m_nodeRules = new HashMap<Node, InspirationRule>();
		m_subGraphs = getSubGraphs();
		
		logger.setLevel(Level.FINEST);
	}

	@Override
	public Set<InspirationRule> build() {
		Set<InspirationRule> rules = new HashSet<InspirationRule>();

		walkCausalChain(getUncausedSet());
		for (InspirationRule ir : m_nodeRules.values()) {
			if (ir != null) {
				rules.add(ir);
			}
		}
		return rules;
	}
	
	protected Set<Quad> getCausalityQuads(Node left, Node right) {

		Set<Quad> causalQuads = new HashSet<Quad>();

		// Search for existing causal links
		for (String cl : BasicRuleBuilder.causalLinks) {

			Iterator it = m_fabulaSet.findQuads(Node.ANY, left, Node
					.createURI(cl), right);
			for (; it.hasNext();) {
				Quad q = (Quad) it.next();
				causalQuads.add(q);
			}
		}
		return causalQuads;
	}

	protected Set<Node> getCauseds(Node causer) {

		Set<Node> causeds = new HashSet<Node>();

		for (String cl : BasicRuleBuilder.causalLinks) {

			Iterator it = m_fabulaSet.findQuads(Node.ANY, causer, Node
					.createURI(cl), Node.ANY);
			for (; it.hasNext();) {
				Quad q = (Quad) it.next();
				causeds.add(q.getObject());
			}
		}

		return causeds;
	}

	protected Set<Quad> getElementQuads(Node elem) {
		/* TODO: if there are predicates (elem fabula:character plop.1) then quads containing plop.1 as subject 
		 should be added too.		 
		 
		 In other words, since we are adding the WHOLE subgraph, we should also add the WHOLE maingraph?
		 makes sense, then you can add setting information that is also required for the rule
		 but then it has to be moved elsewhere
		 
		 HOW to know the difference between 
		 	(goal.24 rdf:type AttainGoal mainGraph)
		 and 
		 	(plop.24 rdf:type Humanoid mainGraph)?
		 	
		 because I WOULD like to know that and include only the second in the "context" of the rule
		 
		 Current solution: getNonFabulaQuads() used in walkCausalChain()
		*/

		Set<Quad> elemQuads = new HashSet<Quad>();

		// FIXME Is this correct? Node.ANY for graph also gives information about elem hidden in some strange subgraph.
		// might work only because the causality makes sure that there is never information about elem in graphs
		// "in the future", i.e., further up the causal chain.
		Iterator it = m_fabulaSet.findQuads(Node.ANY, elem, Node.ANY, Node.ANY);

		for (; it.hasNext();) {
			Quad q = (Quad) it.next();

			// Exclude causal links
			boolean filter = false;
			for (String cl : BasicRuleBuilder.causalLinks) {
				if (q.getPredicate() == Node.createURI(cl)) {
					filter = true;
				}

				if (q.getPredicate() == Node.createURI(Fabula.hasContent)) {

					// also add contents of subgraph (which is the object of the hasContent predicate)
					logger.info("Retrieving content quads for node " + q.getSubject() + ", graph " + q.getObject());
					Iterator it2 = m_fabulaSet.findQuads(q.getObject(),
							Node.ANY, Node.ANY, Node.ANY);
					for (; it2.hasNext();) {
						Quad q2 = (Quad) it2.next();
						elemQuads.add(q2);
					}
					
					// also add information about subgraph
					// FIXME Is this correct? Node.ANY for graph also gives information about elem hidden in some strange subgraph.
					// might work only because the causality makes sure that there is never information about elem in graphs
					// "in the future", i.e., further up the causal chain.
					Iterator it3 = m_fabulaSet.findQuads(Node.ANY, q.getObject(), Node.ANY, Node.ANY);
					for (; it3.hasNext();) {
						Quad q3 = (Quad) it3.next();
						elemQuads.add(q3);
					}

				}
			}

			if (filter == false) {
				// use it
				elemQuads.add(q);

			}
		}
		return elemQuads;
	}

	/*
	 * All quads that do not talk about fabula nodes or graph nodes
	 */
	protected Set<Quad> getNonFabulaQuads() {
		Set<Quad> nfq = new HashSet<Quad>();

		Iterator it = m_fabulaSet.findQuads(Node.ANY, Node.ANY, Node.ANY,
				Node.ANY);
		for (; it.hasNext();) {
			Quad q = (Quad) it.next();
			logger.finer("Checking for non-fabula quad: " + q);
			if (m_fabulaNodes.contains(q.getSubject()) ||
				m_subGraphs.contains(q.getSubject()) || 
			    m_fabulaNodes.contains(q.getObject()) ||
			    m_subGraphs.contains(q.getObject()) ||
			    m_subGraphs.contains(q.getGraphName())) {
				logger.finer("DENIED: m_subGraphs: " + m_subGraphs + "\nq.getGraphName(): " + q.getGraphName());

			} else{
				nfq.add(q);
				logger.finer("ACCEPTED");				
			}
		}

		return nfq;
	}

	/**
	 * Retrieves the set of nodes representing the subgraphs in this named graph set. 
	 * 
	 * @return the set of subgraph nodes
	 */
	protected Set<Node> getSubGraphs() {

		HashSet<Node> subGraphs = new HashSet<Node>();

		/* Implementation: the subgraphs are those graphs that have a fabula:hasContent relation from some node */
		Iterator it = m_fabulaSet.findQuads(Node.ANY, Node.ANY, Node
				.createURI(Fabula.hasContent), Node.ANY);
		for (; it.hasNext();) {
			Quad q = (Quad) it.next();
			subGraphs.add(q.getObject());
		}

		logger.info("Subgraph nodes: " + subGraphs);

		return subGraphs;
	}

	/**
	 * Retrieves the rdf:type of given individual node,
	 * 
	 * @param individual the individual node.
	 * @return the rdf:type of the individual node
	 */
	protected Node getType(Node individual) {
		Iterator it = m_fabulaSet.findQuads(Node.ANY, individual, RDF.type.asNode(), Node.ANY);
		if (it.hasNext()) {
			return ((Quad)it.next()).getObject();
		} else {
			return Node.NULL;
		}
	}

	/**
	 * Retrieves the set of fabula elements that are not caused by anything, i.e. the start of the causal chain(s)
	 * 
	 * @return the set of fabula nodes that are not caused by any other fabula element
	 */
	protected Set<Node> getUncausedSet() {

		HashSet<Node> causers = new HashSet<Node>();
		HashSet<Node> causeds = new HashSet<Node>();

		for (String cl : BasicRuleBuilder.causalLinks) {

			Iterator it = m_fabulaSet.findQuads(Node.ANY, Node.ANY, Node
					.createURI(cl), Node.ANY);
			for (; it.hasNext();) {
				Quad q = (Quad) it.next();
				causers.add(q.getSubject());
				causeds.add(q.getObject());

				m_fabulaNodes.add(q.getSubject());
				m_fabulaNodes.add(q.getObject());
			}
		}

		causers.removeAll(causeds);
		return causers;
	}

	/**
	 * Walks the causal chain of the NGS, starting with given starting points
	 * - Takes the nodes N that the starting points cause, and build a rule from it
	 * - Then, takes N as new starting points
	 * 
	 * @param startingPoints
	 */
	protected void walkCausalChain(Set<Node> startingPoints) {

		logger.info("Starting causal chain walking with nodes: "
				+ startingPoints);

		for (Node left : startingPoints) {
			Set<Node> causeds = getCauseds(left);
			logger.info("working with causeds: " + causeds);

			// build rule
			for (Node right : causeds) {

				logger.info("handling caused: " + right);

				// We are in a situation here where we are looking at a 
				// specific (left causes right) part of the chain.

				// First, build a rule that only contains the NEW and LOCAL information
				// this would contain
				// antecedent: left 
				// consequent: right /\ left causes right
				HashSet<Node> causers = new HashSet<Node>();
				causers.add(left);
				InspirationRule nwRule = new InspirationRule(
						"rule for generating " + right.getLocalName(), right, getType(right), causers,
						m_subGraphs);

				// Add left node to antecedent of the rule
				Set<Quad> elementQuads = getElementQuads(left);
				for (Quad q : elementQuads) {
					logger.fine("Adding to IF part: " + q);
					nwRule.addAntecedent(q);
					//nwRule.addConsequent(q);
				}

				// Add causality to rule
				Set<Quad> causalityQuads = getCausalityQuads(left, right);
				for (Quad q : causalityQuads) {
					//nwRule.addAntecedent(q);
					nwRule.addConsequent(q);
					logger.fine("Adding to THEN part: " + q);					
				}

				Set<Quad> elementQuads2 = getElementQuads(right);
				for (Quad q : elementQuads2) {
					nwRule.addConsequent(q);
					logger.fine("Adding to THEN part: " + q);					
				}

				// Now merge the rule with existing rules
				logger.info("Looking at possible histories...");
				InspirationRule possibleHistoryRule = m_nodeRules.get(left);
				if (possibleHistoryRule != null) {
					logger.info("Copying history into rule.");
					// merge by copying the antecedent AND consequent of earlier rule
					// into antecedent of new rule
					for (Quad q : possibleHistoryRule.getAntecedent()) {
						nwRule.addAntecedent(q);
						logger.fine("Adding to IF part: " + q);
					}
					for (Quad q : possibleHistoryRule.getConsequent()) {
						nwRule.addAntecedent(q);
						logger.fine("Adding to IF part: " + q);
					}
				} else {
					// This is a rule that is "most left": add the non-fabula quads
					logger.info("Copying non-fabula quads into most-left rule.");
					for (Quad q : getNonFabulaQuads()) {
						nwRule.addAntecedent(q);
						logger.fine("Adding to IF part: " + q);
					}
				}

				InspirationRule possibleExistingRule = m_nodeRules.get(right);
				if (possibleExistingRule != null) {
					// merge by adding the rules
					nwRule.add(possibleExistingRule);
				}

				logger.info("Storing new rule: " + nwRule.getName());
				m_nodeRules.put(right, nwRule);
			}

			// Iterate by taking the caused nodes as new starting points
			walkCausalChain(causeds);
		}
	}
}
