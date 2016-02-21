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
package vs.fabula;

import java.io.File;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import org.safehaus.uuid.UUIDGenerator;

import vs.Config;
import vs.communication.FabulaCausality;
import vs.communication.FabulaElement;
import vs.communication.GoalSchema;
import vs.communication.Operator;
import vs.communication.RDFtriple;
import vs.communication.Schema;
import vs.communication.State;
import vs.communication.StoryOutcome;
import vs.debug.FabulaException;
import vs.debug.LogFactory;
import vs.knowledge.vocab.Fabula;
import vs.rationalagent.IRationalAgent;

import com.hp.hpl.jena.graph.Node;
import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.vocabulary.RDF;

public class BasicFabulaBuilder implements IFabulaBuilder {

	//public static String defaultGraphURI = "http://www.owl-ontologies.com/Graphs.owl#";
	public static String defaultGraphURI = "";

	public static String defaultGraphName = BasicFabulaBuilder.defaultGraphURI
			.concat("#defaultGraph");

	protected IRationalAgent ownerAgent;

	protected OntModel refModel;

	protected UUIDGenerator idGen;
	
	protected Logger logger;
	
	protected HashSet<FabulaKnowledgeBase> m_fabulaStorageSet;
	protected Set<String> handledIndividuals;

	public BasicFabulaBuilder(IRationalAgent owner) {
		
		logger = LogFactory.getLogger(this);

		ownerAgent = owner;
		
		handledIndividuals = new HashSet<String>();

		// Register fabula storages
		m_fabulaStorageSet = new HashSet<FabulaKnowledgeBase>();

		idGen = UUIDGenerator.getInstance();

		refModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
		for (Map.Entry e : Config.namespaceMap.entrySet()) {
			logger
					.fine("Adding namespace\n" + e.getKey() + "\n"
							+ e.getValue());
			refModel
					.setNsPrefix(e.getKey().toString(), e.getValue().toString());
			logger.fine("rdf:type expanded: "
					+ refModel.expandPrefix("rdf:type"));
		}

		//refModel.setNsPrefixes(Config.namespaceMap);

	}

	/**
	 * Draw a causal connection between fabula elements.
	 * 
	 * @param elem1 the source element
	 * @param cause the causation
	 * @param elem2 the target element
	 * 
	 * Example: addCausality("fabula:goal.324", "fabula:motivates", "fabula:action112");
	 * 
	 * @return whether adding succeeded
	 */
	protected boolean addCausality(String elem1, String cause, String elem2)
			throws FabulaException {

		boolean val = true;

		for (FabulaKnowledgeBase fs : m_fabulaStorageSet) {
			val &= fs.addTriple(elem1, cause, elem2);
		}
		return val;
	}

	/**
	 * Adds a fabula element to the fabula. Note: this doesn't mean it is a top-level element, it can also
	 * be contained in a subgraph. 
	 *  
	 * @param type
	 * @return
	 */
	protected String addElement(String type, String individual)
			throws FabulaException {
		//String nwInd = UniqueId.generateUniqueIndividual(type, ID);

		if (type == null || individual == null) {
			throw new FabulaException(
					"Individual or type are null.\nIndividual: " + individual
							+ "\nType: " + type);
		}
		
		if (handledIndividuals.contains(individual)) {
			throw new FabulaException(
					"Duplicate entry of individual! This should only happen if the new entry contains additional information, " +
					"e.g. the endtime of an action is now known.\nIndividual: " + individual);
		}

		for (FabulaKnowledgeBase fs : m_fabulaStorageSet) {
			
			// Add type of fabula element
			fs.addTriple(
					Node.createURI(refModel.expandPrefix(individual)),
					RDF.type,
					Node.createURI(refModel.expandPrefix(type)));

		}

		return individual;
	}
	
	public void addFabulaCausality(FabulaCausality fc) {
		logger.fine("Handling fabula causality: (" + fc.getSubjectIndividual() + "," + fc.getCausalProperty() + "," + fc.getObjectIndividual() + ")");
		try {
			addCausality(fc.getSubjectIndividual(), fc.getCausalProperty(), fc
					.getObjectIndividual());
		} catch (FabulaException fe) {
			logger.warning(fe.getLocalizedMessage());
		}
	}

	public void addFabulaElement(FabulaElement fe) {
		try {
			// Add type and individual
			addElement(fe.getType(), fe.getIndividual());
			
			// Add character of fabula element
			addTripleToKBs(fe.getIndividual(), Fabula.character, fe.getCharacter());

			// Add time of fabula element. If it is specified in the fabula element, use that value. Otherwise, take current time
			Integer time;
			if (fe.getTime() > 0) {
				time = new Integer(fe.getTime());
			} else {
				time = new Integer(ownerAgent.getTime());
			}
			addTripleToKBs(
					Node.createURI(refModel.expandPrefix(fe.getIndividual())),
					Node.createURI(refModel.expandPrefix(Fabula.time)), 
					refModel.createTypedLiteral(time));
			
			// Register as "handled"
			handledIndividuals.add(fe.getIndividual());
		} catch (FabulaException ex) {
			logger.warning(ex.getLocalizedMessage());
		}

		/* ****************** 
		 * Handle Operator 
		 * *****************/
		if (fe instanceof Operator) {
			Operator o = (Operator) fe;
			logger.fine("Handling new fabula element (Operator): " + o.getIndividual());

			addTripleToKBs(o.getIndividual(), Fabula.agens, o.getAgens());

			addTripleToKBs(o.getIndividual(), Fabula.patiens, o.getPatiens());

			addTripleToKBs(o.getIndividual(), Fabula.target, o.getTarget());

			addTripleToKBs(o.getIndividual(), Fabula.instrument, o
						.getInstrument());

			// Log start time
			int starttime = o.getStarttime();
			if (starttime > 0) {
				addValue(
						o.getIndividual(), Fabula.starttime,
						new Integer(o.getStarttime()));
				
				// If there's a start time, also try to log end time
				int endtime = o.getEndtime(); 
				if (endtime >= starttime) {
					addValue(
							o.getIndividual(), Fabula.endtime,
							new Integer(o.getEndtime()));
				}
			}
						
		}

		/* ****************** 
		 * Handle State 
		 * *****************/
		if (fe instanceof State) {
			State s = (State) fe;
			logger.finer("Handling new fabula element (State): " + s.getIndividual());
			int trueCount = 0;
			int falseCount = 0;


			// Make a new graph name
			// TODO: how are contents represented if they are, again, fabula elements?
			String graphName = BasicFabulaBuilder.defaultGraphURI.concat(s.getIndividual()).concat("_contents");

			// Add a hasContext relationship to the new graph name
			addTripleToKBs(s.getIndividual(), Fabula.hasContent, graphName);

			// Add the contextualized triples to the new graph
			for (Iterator it = s.getContentTriple().iterator(); it
					.hasNext();) {

				RDFtriple t = (RDFtriple) it.next();
				if (t.getTruth()) {
					trueCount++;
				} else {
					falseCount++;
				}

				addQuadToKBs(graphName, t.getSubject(), t.getPredicate(), t
						.getObject());
			}

			try {
				if (trueCount > 0 && falseCount > 0) {
					throw new FabulaException("Fabula element contains both true and false statements. Should not happen!");	
				}
				
				if (trueCount == 0 && falseCount == 0) {
					throw new FabulaException("Fabula element contains no (true or false) statements. Should not happen!");
				}

				if (trueCount > 0) {
					addTripleToKBs(graphName, RDF.type, Fabula.TruthGraph);
				} else {
					addTripleToKBs(graphName, RDF.type, Fabula.FalsehoodGraph);
					
				} 
			} catch (FabulaException ex) {
				logger.warning(ex.getLocalizedMessage());
			}

		}
		
		/* ****************** 
		 * Handle StoryOutcome 
		 * *****************/
		if (fe instanceof StoryOutcome) {
			StoryOutcome so = (StoryOutcome) fe;
			logger.finer("Handling new fabula element (StoryOutcome): " + so.getIndividual());
			String resolves = so.getResolves();
			try {
				if (resolves == null) {
					throw new FabulaException(	
							"Outcome does not point to a goal it resolves.");
				}
				addTripleToKBs(so.getIndividual(), Fabula.resolves, resolves);
			} catch (FabulaException ex) {
				logger.warning(ex.getLocalizedMessage());
			}

		}
		
		/* ****************** 
		 * Handle Schema 
		 * *****************/
		if (fe instanceof Schema) {
			if (fe instanceof GoalSchema) {
				GoalSchema gs = (GoalSchema)fe;
				logger.finer("Handling new fabula element (GoalSchema): " + gs.getIndividual());
				
				addTripleToKBs(gs.getIndividual(), Fabula.agens, gs.getAgens());

				addTripleToKBs(gs.getIndividual(), Fabula.patiens, gs.getPatiens());

				addTripleToKBs(gs.getIndividual(), Fabula.target, gs.getTarget());

				addTripleToKBs(gs.getIndividual(), Fabula.instrument, gs.getInstrument());
				
				
				// TODO
				
/*				try {
					for (Iterator it = gs.getAllParameter(); it.hasNext(); ) {
						Parameter p = (Parameter)it.next();
						if (p instanceof Agens) {
							addTriple(fe.getIndividual(), Fabula.agens, p.getValue());
						} else if (p instanceof Patiens) {
							addTriple(fe.getIndividual(), Fabula.patiens, p.getValue());
						} else if (p instanceof Target) {
							addTriple(fe.getIndividual(), Fabula.target, p.getValue());
						} else if (p instanceof Instrument) {
							addTriple(fe.getIndividual(), Fabula.instrument, p.getValue());
						} else {
							throw new FabulaException("Parameter " + p.getClass() + " not implemented yet in Fabula builder.");
						}
					}
				} catch (FabulaException ex) {
					logger.warning(ex.getLocalizedMessage());
				}
*/				
			}
		}

	}

	protected boolean addQuadToKBs(
			Object graph, 
			Object elem1, 
			Object rel, 
			Object elem2) {
		boolean succ = true;
		for (FabulaKnowledgeBase fs: m_fabulaStorageSet) {
			succ &= fs.addQuad(graph, elem1, rel, elem2);
		}
		return succ;
	}


	/* --------------------------------------------------
	 * PRIVATE METHODS -- INTERNAL WORKINGS
	 * --------------------------------------------------
	 */	
	
	protected boolean addTripleToKBs(
			Object elem1, 
			Object rel, 
			Object elem2) {
		boolean succ = true;
		for (FabulaKnowledgeBase fs : m_fabulaStorageSet) {
			succ &= fs.addTriple(elem1, rel, elem2);
		}
		return succ;
	}
	
	protected void addValue(String individual, String rel, Object value) {

		for (FabulaKnowledgeBase fs : m_fabulaStorageSet) {
			fs.addTriple(Node.createURI(individual), Node.createURI(
					rel), refModel.createTypedLiteral(value));
		}

	}
	
	public IRationalAgent getAgent() {
		return ownerAgent;
	}

	public Set<FabulaKnowledgeBase> getAllFabulaKnowledgeBase() {
		return m_fabulaStorageSet;
	}

	public void registerFabulaKnowledgeBase(FabulaKnowledgeBase storage) {
		m_fabulaStorageSet.add(storage);
	}

	public void saveFabula(File filename, String language) {

		// FIXME this is weird. We only want one file. Make fs.saveFabula() boolean?
		for (FabulaKnowledgeBase fs : m_fabulaStorageSet) {
			fs.saveFabula(filename, language);
		}
	}

}
