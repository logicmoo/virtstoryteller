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
package vs.plotagent;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.logging.Logger;

import org.jpl7.PrologException;
import org.jpl7.Term;
import vs.communication.RDFtriple;
import vs.communication.StoryAction;
import vs.communication.StoryEvent;
import vs.communication.StoryGoal;
import vs.debug.LogFactory;
import vs.knowledge.PrologKB;
import vs.knowledge.vocab.Fabula;
import vs.plotagent.inspiration.BasicCaseTransformer;
import vs.plotagent.inspiration.BasicRuleBuilder;
import vs.plotagent.inspiration.CaseTransformer;
import vs.plotagent.inspiration.InspirationRule;
import vs.plotagent.inspiration.OperationalizedSuggestion;
import vs.plotagent.inspiration.RuleBuilder;
import vs.plotagent.inspiration.Suggestion;
import vs.utils.UniqueId;

import com.hp.hpl.jena.ontology.OntModel;
import com.hp.hpl.jena.rdf.model.ModelFactory;

import de.fuberlin.wiwiss.ng4j.NamedGraphSet;
import de.fuberlin.wiwiss.ng4j.impl.NamedGraphSetImpl;

public class BasicInspirationModule implements IInspirationModule {

	protected IPlotAgent m_ownerAgent;

	protected Logger logger;

	protected Map<InspirationRule, RuleBuilder> ruleBuilderMap;
	
	protected Set<String> usedSuggestionRules;

	/**
	 * Constructor
	 * 
	 * @param ownerAgent the agent owning this module
	 */
	public BasicInspirationModule(IPlotAgent ownerAgent) {

		logger = LogFactory.getLogger(this);

		m_ownerAgent = ownerAgent;
		
		usedSuggestionRules = new HashSet<String>();

		//	m_ownerAgent.getKnowledgeManager().consult(Config.PROLOGFILESPATH + "narrative-inspiration.pl");
	}

	/* See Interface */
	public void addUsedSuggestion(Suggestion s) {
		usedSuggestionRules.add(s.getRuleName());
	}

	/* See Interface */
	public Set<InspirationRule> generateRules(String fabulaCaseURL) {

		// Open fabula case file
		String ruleFile = "";
		Set<InspirationRule> ruleSet = new HashSet<InspirationRule>();

		NamedGraphSet ngs = new NamedGraphSetImpl();
		ngs.read(fabulaCaseURL, "TRIG");

		// Transform cases
		CaseTransformer transformer = new BasicCaseTransformer(ngs);

		// For each (possibly transformed) case, generate the rules
		for (NamedGraphSet ngCase : transformer.transform()) {

			RuleBuilder builder = new BasicRuleBuilder(ngCase);

			// Build each rule
			logger.info("Building rule output...");
			ruleSet.addAll(builder.build());

		}

		logger.info("Rule building done: " + ruleSet.size()
				+ " rules built from file " + fabulaCaseURL + ".");

		return ruleSet;
	}

	/* See Interface */
	public IPlotAgent getAgent() {

		return m_ownerAgent;
	}

	/* See Interface*/
	/*	public Set<Suggestion> getSuggestions() {
			
			Variable suggestionName = new Variable("SuggestionName");
			Variable suggestionResult = new Variable("FabulaChain");
			
			Query suggQuery = m_ownerAgent.getKnowledgeManager()
									.prolog("suggestion(" + 
											suggestionName + "," + 
											suggestionResult + ").");
			
			if (!suggQuery.hasSolution()) {
				logger.info("No suggestions.");
				return new HashSet<Suggestion>();
			}
			
			Set<Suggestion> suggestionSet = new HashSet<Suggestion>();
			while ( suggQuery.hasMoreElements() ) {
				Hashtable binding = (Hashtable) suggQuery.nextElement();
				Term res = (Term) binding.get( suggestionResult.toString());
				Term name = (Term) binding.get( suggestionName.toString());
				if (res != null && name != null) {
					Suggestion s = new Suggestion(res.toString(), name.toString());
					suggestionSet.add(s);
					logger.info("Returning suggestion based on rule " + binding.get( suggestionName.toString()));
				}
			}
			
			return suggestionSet;
			
		}*/

	/*	 See Interface.
	 * 
	 * Policy for now: a rule can only be used once.
	 * */
	public Set<Suggestion> getSuggestions() {

		Set<Suggestion> suggestionSet = new HashSet<Suggestion>();

		Vector<String> suggestions = m_ownerAgent.getKnowledgeManager()
				.getPrologSingleVariableList(PrologKB.getSuggestion);
		int initial = suggestions.size();

		for (String suggestion : suggestions) {
			// A suggestion String is still a Prolog list.
			String suggName = getAgent().getKnowledgeManager()
					.getPrologSingleVariable(PrologKB.getSuggestionName,
							suggestion);
			
			String suggIndividual = getAgent().getKnowledgeManager()
					.getPrologSingleVariable(PrologKB.getSuggestionIndividual,
					suggestion);
			
			String suggType = getAgent().getKnowledgeManager()
					.getPrologSingleVariable(PrologKB.getSuggestionType,
					suggestion);
			
			String suggCausers = getAgent().getKnowledgeManager()
					.getPrologSingleVariable(PrologKB.getSuggestionCausers,
					suggestion);			
			
			String suggBody = getAgent().getKnowledgeManager()
					.getPrologSingleVariable(PrologKB.getSuggestionBody,
							suggestion);
			
			// Check if suggestion rule was already used
			if (usedSuggestionRules.contains(suggName)) {
				logger.fine("Rule " + suggName + " already used; skipping it.");
			} else {
				// If so, create suggestion
				if (!(suggName.equals("")) && !(suggBody.equals(""))) {
					logger.fine("Creating suggestion based on rule " + suggName);
					
					// decompose suggCausers
					Set<String> suggCauserSet = new HashSet<String>();
					
					Term t = org.jpl7.Util.textToTerm(suggCausers);
					Term[] causers = t.toTermArray();
					
					for (Term causer: causers) {
					
						suggCauserSet.add(causer.toString());
					}
					
					Suggestion s = new Suggestion(suggName, suggIndividual, suggType, suggCauserSet, suggBody);
					suggestionSet.add(s);
					
					// Remember that suggestion has been used.
					//usedSuggestionRules.add(suggName);
				}
			}
		}
		logger.info("Prolog gives " + initial + " suggestions, " + suggestionSet.size() + " unused.");
		
		return suggestionSet;
	}
	
	/* See Interface 
	 * @Deprecated, loading rules should be done in Prolog somewhere, consulting them INTO the library
	 * */
	@Deprecated
	public void loadRules(String filename) {
		m_ownerAgent.getKnowledgeManager().consult(filename);
	}
	
	private Set<OperationalizedSuggestion> operationalizeAction(Suggestion sug) {
		Set<OperationalizedSuggestion> oSugSet = new HashSet<OperationalizedSuggestion>();
		
		Vector<String> validatedActions = getAgent().getKnowledgeManager()
			.getPrologSingleVariableList(
				PrologKB.createValidatedAction,
				sug.getBody() + "," + sug.getIndividual());
		
		String fabulaCharacter = getAgent().getKnowledgeManager().getPrologSingleVariable(
				PrologKB.getFabulaCharacter, 
				sug.getBody() + "," + sug.getIndividual());

		logger.info("Operationalizing action leads to " + validatedActions.size() + " Actions.\n" + sug.getBody());
		for (String act: validatedActions) {
			OperationalizedSuggestion oSug = new OperationalizedSuggestion(sug);
		
			// Create event
			StoryAction sa = new StoryAction();

			sa.setCharacter(PrologKB.removeQuotes(fabulaCharacter));
			sa.setPrologDescription(act);
			sa.setIndividual(UniqueId.generateUniqueIndividual("sugAction", "plotagent"));
			sa.setType(PrologKB.getInstance().getSchemaType(act));
			sa.setAgens(PrologKB.getInstance().getSchemaAgens(act));
			sa.setPatiens(PrologKB.getInstance().getSchemaPatiens(act));
			sa.setTarget(PrologKB.getInstance().getSchemaTarget(act));
			sa.setInstrument(PrologKB.getInstance().getSchemaInstrument(act));
			
			oSug.setOperationalizedElement(sa);
			
			oSugSet.add(oSug);
		}

		
		return oSugSet;
	}
	
	private Set<OperationalizedSuggestion> operationalizeEvent(Suggestion sug) {
		Set<OperationalizedSuggestion> oSugSet = new HashSet<OperationalizedSuggestion>();
		
		Vector<String> validatedEvents = getAgent().getKnowledgeManager()
			.getPrologSingleVariableList(
				PrologKB.createValidatedEvent,
				sug.getBody() + "," + sug.getIndividual());
		
		logger.info("Operationalizing event leads to " + validatedEvents.size() + " Events.\n" + sug.getBody());
		for (String ev: validatedEvents) {
			OperationalizedSuggestion oSug = new OperationalizedSuggestion(sug);
		
			// Create event
			StoryEvent se = new StoryEvent();

			// TODO: what is the fabula:character when it is the plot agent "doing" it?
			se.setCharacter("plotagent");
			se.setPrologDescription(ev);
			se.setIndividual(UniqueId.generateUniqueIndividual("sugEvent", "plotagent"));
			se.setType(PrologKB.getInstance().getSchemaType(ev));
			se.setAgens(PrologKB.getInstance().getSchemaAgens(ev));
			se.setPatiens(PrologKB.getInstance().getSchemaPatiens(ev));
			se.setTarget(PrologKB.getInstance().getSchemaTarget(ev));
			se.setInstrument(PrologKB.getInstance().getSchemaInstrument(ev));
			
			oSug.setOperationalizedElement(se);
			
			oSugSet.add(oSug);
		}

		
		return oSugSet;
	}	
	
	private Set<OperationalizedSuggestion> operationalizeGoal(Suggestion sug) {
		Set<OperationalizedSuggestion> oSugSet = new HashSet<OperationalizedSuggestion>();
		// TODO
		// ...
		// Create goal
		StoryGoal sg = new StoryGoal();

		String goalCharacter = getAgent().getKnowledgeManager()
				.getPrologSingleVariable(PrologKB.getSuggestionCausers, sug.getBody());
		
		sg.setCharacter(PrologKB.removeQuotes(goalCharacter));
		
		sg.setIndividual(UniqueId.generateUniqueIndividual("sugGoal", "plotagent"));
		sg.setType(PrologKB.removeQuotes(sug.getType()));
		String goalContents = getAgent().getKnowledgeManager().getPrologSingleVariable(
				PrologKB.getFabulaContents, 
				sug.getBody() + "," + sug.getIndividual());
		
		String goalTruth = getAgent().getKnowledgeManager().getPrologSingleVariable(
				PrologKB.getFabulaContentTruth, 
				sug.getBody() + "," + sug.getIndividual());
		goalTruth = PrologKB.removeQuotes(goalTruth);
		
		// Default (when information lacks): true
		boolean truth = true;
		
		// Parse if information is given
		if (!goalTruth.equals("")) {
			// This part on suggestion of Dave Reynolds (jena-dev list 28-8-07)
			OntModel literalMaker = ModelFactory.createOntologyModel();
			int begn = goalTruth.indexOf("^^");
			truth = literalMaker.createTypedLiteral(goalTruth.substring(0, begn).replace('"', ' ').trim(), goalTruth.substring(begn+2)).getBoolean();

			/*Alternative (but don't know how to proceed from there:
			 * note: getting a BaseDataType.TypedValue means the parsing failed; try removing the quotes (") first. Reynolds 28-8-07
			 	Object value = TypeMapper.getInstance()
        			.getSafeTypeByName(goalTruth.substring(0, begn))
        			.parse(goalTruth.substring(begn+2));*/
			
			// goalTruth = literalMaker.createTypedLiteral(new Boolean(true)).asNode().toString();
		}
		
		logger.fine("Query: " + PrologKB.getFabulaContents + "(" + sug.getBody() + "," + sug.getIndividual() + ",FabulaContents).");

		//logger.fine("Boolean value: " + truth);
		try {
			Term term = org.jpl7.Util.textToTerm(goalContents);
			Term[] goalContentTriples = term.toTermArray();

			for (Term goalContentTriple: goalContentTriples) {
				logger.fine("Goal content triples: " + goalContentTriple);
				// TODO: content might be fabula again; needs to be operationalized too for StoryGoal.addContentFabula()
				
				RDFtriple t = PrologKB.getInstance().prologToTriple(goalContentTriple.toString(), truth);
/*				t.setSubject(getAgent().getKnowledgeManager().getQNPF(PrologKB.getSubject, goalContentTriple.toString()));
				t.setPredicate(getAgent().getKnowledgeManager().getQNPF(PrologKB.getPredicate, goalContentTriple.toString()));
				t.setObject(getAgent().getKnowledgeManager().getQNPF(PrologKB.getObject, goalContentTriple.toString()));
				t.setTruth(truth);/*
		
		/*			if (l instanceof Literal) {
						b = ((Literal)l).getBoolean();
						logger.info("Setting b to " + b);
					} else {
						logger.info(l + " is no literal.");
					}*/
				
				if (t==null) {
					logger.warning("Retrieved goal content triple is null!");
				} else {
					logger.fine("Goal content triple: \n" + "subject: " + t.getSubject() + "\npredicate: " + t.getPredicate() + "\nobject: " + t.getObject() + "\ntruth: " + truth);					
					sg.addContentTriple(t);
				}
			}
			
			OperationalizedSuggestion oSug = new OperationalizedSuggestion(sug);
			oSug.setOperationalizedElement(sg);
			oSugSet.add(oSug);
		
		} catch (PrologException pe) {
			// FIXME: strange, this only occurs with character agents that were launched by the plot agent in stead of manually.
			logger.severe("Could not translate goal contents to term array: \n" + goalContents);
			pe.printStackTrace();
		} 
		
		return oSugSet;
	}	
	
	/* See Interface */
	public Set<OperationalizedSuggestion> operationalizeSuggestions(Set<Suggestion> suggestions) {
		Set<OperationalizedSuggestion> operSuggestions = new HashSet<OperationalizedSuggestion>();
		
		for (Suggestion sug: suggestions) {
			boolean uncaught = true;
			Set<OperationalizedSuggestion> oSug = null;
			
			// Is it an Event?
			logger.fine("Query to ask whether suggestion is about an Event: ");
			StringBuilder queryBuilder = new StringBuilder();
			queryBuilder.append(PrologKB.nodeClass).append('(').append(sug.getBody()).append(",'")
						.append(Fabula.Event).append("',").append(sug.getIndividual()).append(").");
			
			if (uncaught && getAgent().getKnowledgeManager().ask(queryBuilder.toString())) {
				// it's an event!
				logger.fine("Event suggestion!");
				oSug = operationalizeEvent(sug);
				uncaught = false;
			}
			
			
			// Is it an Action?
			logger.fine("Query to ask whether suggestion is about an Action: ");
			queryBuilder = new StringBuilder();
			queryBuilder.append(PrologKB.nodeClass).append('(').append(sug.getBody()).append(",'")
						.append(Fabula.Action).append("',").append(sug.getIndividual()).append(").");
			
			if (uncaught && getAgent().getKnowledgeManager().ask(queryBuilder.toString())) {
				// it's an event!
				logger.fine("Action suggestion!");
				oSug = operationalizeAction(sug);
				uncaught = false;
			}			
			
			// Is it a Goal?
			logger.fine("Query to ask whether suggestion is about a Goal: ");			
			queryBuilder = new StringBuilder();
			queryBuilder.append(PrologKB.nodeClass).append('(').append(sug.getBody()).append(",'")
						.append(Fabula.Goal).append("',").append(sug.getIndividual()).append(").");
			
			if (uncaught && getAgent().getKnowledgeManager().ask(queryBuilder.toString())) {
				// it's an event!
				logger.fine("Goal suggestion!");
				oSug = operationalizeGoal(sug);
				uncaught = false;
			}		
			
			// add communication object
			if (oSug != null) {
				operSuggestions.addAll(oSug);
			}
		}
		
		return operSuggestions;
	}

	public void writeRules(Set<InspirationRule> rules, String ruleFileURL) {
		String ruleFile =  "suggestions/suggestions-" + ruleFileURL;

		try {
			FileOutputStream fos = new FileOutputStream(ruleFile);

			// Write the rules to file
			for (InspirationRule ir : rules) {
				fos.write(ir.toString().getBytes());
				fos.write("\n\n".getBytes());
			}

			fos.close();

		} catch (FileNotFoundException e) {
			logger.severe("Cannot open file for writing: " + rules);
		} catch (IOException e) {
			logger.severe("Error writing to file: " + ruleFileURL);
		} catch (SecurityException e) {
			logger
					.severe("There is a security problem: no write access to file "
							+ ruleFileURL);
		}

		logger.info("Rules written to file " + ruleFile + ".");
	}

}
