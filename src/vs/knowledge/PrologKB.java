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
package vs.knowledge;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.jpl7.Atom;
import org.jpl7.Compound;
import org.jpl7.JPLException;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;
import org.jpl7.Variable;

import vs.communication.GoalSchema;
import vs.communication.Operator;
import vs.communication.RDFtriple;
import vs.debug.LogFactory;
import vs.poplanner.PlanLink;
import vs.poplanner.PlanOrdering;
import vs.poplanner.PlanStep;

/**
 * Contains all methods that have to call Prolog for their functionality. Do _NOT_ use any call to Prolog
 * anywhere else than here. The idea is that this class provides _the_ interface to Prolog. 
 * 
 * @author swartjes 
 */
public class PrologKB {

	// Modules
	protected static final String rdf_db 				= "rdf_db:";
	protected static final String schema_management 	= ""; // "schema_management:";
	protected static final String thread_management 	= ""; // "thread_management:";
	protected static final String goal_management 		= ""; // "goal_manager:";
	protected static final String reactive_layer 		= ""; // "reactive_layer:";
	protected static final String planner 				= "planner:";
	protected static final String knowledgebase 		= ""; // "knowledgebase:";
	protected static final String narrator 				= ""; // "narrator:";
	protected static final String narrative_inspiration = ""; // "narrative_inspiration:";
	protected static final String characterAgent 		= ""; // "basicCharacterAgent:";
	protected static final String worldAgent 			= ""; // "basicCharacterAgent:";
	
	// TODO: Below predicates should all be made protected. Other classes should not be able to reference them.
	
	// -------------------------
	// General prolog predicates
	// -------------------------
	protected static final String consult 	= "consult";
	public static final String member 		= "member";
	protected static final String rdf_load 	= PrologKB.rdf_db + "rdf_load";
	public static final String length 		= "length";
	
	// -----------------------	
	// Character agent module 
	// -----------------------
	protected static final String setAgentID 			= characterAgent + "setAgentID";
	protected static final String hasAction 			= characterAgent + "hasAction";
	protected static final String canDo 				= characterAgent + "canDo";
	protected static final String goal_intention		= characterAgent + "goal_intention";
	protected static final String schema_enablement		= characterAgent + "schema_enablement";
	protected static final String goal_motivation		= characterAgent + "goal_motivation";
	protected static final String possible_goal_after_plan = characterAgent + "possible_goal_after_plan";
	
	// -----------------------	
	// World agent module 
	// -----------------------
	protected static final String check_schema_facts 	= worldAgent + "check_schema_facts";
	protected static final String operator_effect 		= worldAgent + "operator_effect";
	
	// ------------------------
	// Schema management module
	// ------------------------

	protected static final String applyOperatorEffects 		= schema_management + "apply_operator_effects";
	protected static final String getSchemaPreconditions    = schema_management + "schema_preconditions";
	protected static final String getSchemaKind				= schema_management + "schema_kind";
	protected static final String getOperatorType 			= schema_management + "schema_type";
	protected static final String getOperatorAgens 			= schema_management + "schema_agens";
	protected static final String getOperatorPatiens 		= schema_management + "schema_patiens";
	protected static final String getOperatorTarget 		= schema_management + "schema_target";
	protected static final String getOperatorOpponent		= schema_management + "schema_opponent";
	protected static final String getOperatorInstrument 	= schema_management + "schema_instrument";
	protected static final String getOperatorDuration		= schema_management + "operator_duration";
	protected static final String getGoalUrgency 			= schema_management + "goal_urgency";
	protected static final String getFramingScopeAll		= schema_management + "framing_scope_all";
	protected static final String getFramingScopePersonal	= schema_management + "framing_scope_personal";
	protected static final String getFramingScopeHidden		= schema_management + "framing_scope_hidden";
	protected static final String validateSchema 			= schema_management + "validate_schema";
	protected static final String validateGoalFailure		= schema_management + "validate_goal_failure_conditions";
	
	// ---------------------
	// Knowledge base module
	// ---------------------

	protected static final String rdfAssert 	= knowledgebase + "rdfAssert";
	protected static final String rdfRetract 	= knowledgebase + "rdfRetract";
	protected static final String query			= knowledgebase + "query";
	protected static final String first 		= knowledgebase + "first";
	protected static final String second 		= knowledgebase + "second";
	protected static final String getSubject 	= knowledgebase + "getSubject";
	protected static final String getPredicate 	= knowledgebase + "getPredicate";
	protected static final String getObject 	= knowledgebase + "getObject";

	// ----------------------------
	// Narrative inspiration module
	// ----------------------------
	public static final String getSuggestion 			= narrative_inspiration + "getSuggestion";
	public static final String getSuggestionName 		= narrative_inspiration + "getSuggestionName";
	public static final String getSuggestionIndividual	= narrative_inspiration + "getSuggestionIndividual";
	public static final String getSuggestionType 		= narrative_inspiration + "getSuggestionType";
	public static final String getSuggestionCausers 	= narrative_inspiration + "getSuggestionCausers";
	public static final String getSuggestionBody 		= narrative_inspiration + "getSuggestionBody";
	public static final String nodeClass 				= narrative_inspiration + "nodeClass";
	public static final String causalityClass 			= narrative_inspiration + "causalityClass";
	public static final String fabulaNode 				= narrative_inspiration + "fabulaNode";
	public static final String fabulaCause 				= narrative_inspiration + "fabulaCause";
	public static final String getFabulaCharacter 		= narrative_inspiration + "getFabulaCharacter";
	public static final String getFabulaContents 		= narrative_inspiration + "getFabulaContents";
	public static final String getFabulaContentTruth 	= narrative_inspiration + "getFabulaContentTruth";
	public static final String createValidatedEvent 	= narrative_inspiration + "createValidatedEvent";
	public static final String createValidatedAction 	= narrative_inspiration + "createValidatedAction";

	// -----------------------------
	// Partial order planning module
	// -----------------------------
	protected static final String plan						= planner + "plan";
	protected static final String adaptPlan					= planner + "adapt_plan";
	protected static final String invalidatesPlan			= planner + "invalidates_plan";
	protected static final String invalidPlan				= planner + "invalid_plan";
	protected static final String executableOperator		= planner + "executableOperator";
	protected static final String executableFramingOperator	= planner + "executableImprovisation";
	protected static final String executableInferenceOperator	= planner + "executableInference";
	protected static final String executableEvent			= planner + "executableEvent";
	protected static final String finishedPlan			= planner + "finished_plan";

	protected static final String planStep 				= planner + "planStep";
	protected static final String planOrdering 			= planner + "planOrdering";
	protected static final String planLink 				= planner + "planLink";
	protected static final String planLinkFrom 			= planner + "planLinkFrom";
	protected static final String planLinkTo 			= planner + "planLinkTo";
	protected static final String planLinkCond 			= planner + "planLinkCond";
	
	// -------------------------
	// Episode management module
	// -------------------------
	protected static String possibleThread 		= thread_management + "possible_thread";
//	protected static String startEpisode 			= episode_management + "start_episode";
	protected static String necessaryCharacter 		= thread_management + "necessary_character";
//	protected static String castedCharacter 		= episode_management + "casted_character";
	protected static String threadGoal 			= thread_management + "thread_goal";
	protected static String threadResolveGoal 			= thread_management + "thread_resolve_goal";	
	protected static String threadSetting 		= thread_management + "thread_setting";
	protected static String condition_to_triples 	= thread_management + "condition_to_triples";
	
	// ----------------------
	// Goal management module
	// ----------------------
	protected static final String possible_goal				= goal_management + "possible_goal";
	protected static final String adopt_goal				= goal_management + "adopt_goal";
	protected static final String drop_goal					= goal_management + "drop_goal";
	protected static final String adopt_justifiable_goal	= goal_management + "adopt_justifiable_goal";
	protected static final String adopted_goal				= goal_management + "adopted_goal";
	protected static final String adopted_justifiable_goal	= goal_management + "adopted_justifiable_goal";
	protected static final String suggest_goal				= goal_management + "suggest_goal";
	protected static final String suggested_goal			= goal_management + "suggested_goal";
	
	// ---------------------
	// Reactive layer module
	// ---------------------
	protected static final String select_action_reactively	= reactive_layer + "select_action";
	
	// -------------------------
	// Narrator module
	// -------------------------
	protected static String narrate 	= narrator + "narrate";
	
	protected static PrologKB M_KNOWLEDGEMANAGER = null;

	public static String addQuotes(String input) {
		StringBuilder sb = new StringBuilder().append('\'').append(input)
				.append('\'');
		return sb.toString(); //"'" + input + "'";
	}
	
	/**
	 * Substring from '#'
	 * @param input a String containing a '#'
	 * @return the string after the first '#'
	 */
	public static String fromNrSign(String input) {
		return input.substring(input.indexOf("#") + 1);
	}

	public static PrologKB getInstance() {
		if (M_KNOWLEDGEMANAGER == null) {
			M_KNOWLEDGEMANAGER = new PrologKB();
		}
		return M_KNOWLEDGEMANAGER;
	}
	
	public static String listToProlog(List<RDFtriple> l) {
		boolean notFirst = false;
		if (l == null) return "[]";
		
		StringBuilder sb = new StringBuilder();
		sb.append('[');
		for (RDFtriple t: l) {
			if (notFirst) {
				sb.append(',');
			}
			sb.append(tripleToProlog(t));
			notFirst = true;
		}
		sb.append(']');
		return sb.toString();
	}
	
	/**
	 * Remove the single quotes from a Prolog value 
	 * @param input the string with potential quotes
	 * @return a string with quotes removed
	 */
	public static String removeQuotes(String input) {
		return input.replace('\'', ' ').trim();
	}

	/**
	 * Turns an RDF triple Java object back into a Prolog fact
	 * @param t the RDF triple in Java

	 * @return a Prolog string representing the triple
	 */	
	public static String tripleToProlog(RDFtriple t) {
		StringBuilder sb = new StringBuilder();
		if (t == null) return "";
		Atom subj = new Atom(t.getSubject());
		Atom pred = new Atom(t.getPredicate());
		Atom obj = new Atom(t.getObject());
		sb.append("(").append(subj).append(",").append(
				pred).append(",").append(obj).append(
				")");
		return sb.toString();
	}

	protected final Logger logger;	
	
	/**
	 * Singleton pattern's private constructor
	 */
	private PrologKB() {

		// Initialize logger
		logger = LogFactory.getLogger(this);

	}
	
	/**
	 * Tell Prolog to adopt a goal. This makes it no longer eligible for goal selection
	 * 
	 * @param goal the Prolog string representing the goal to adopt
	 */
	public boolean adoptGoal(String goal) {
		return call(PrologKB.adopt_goal, goal);
	}
	
	/**
	 * Tell Prolog to drop a goal. 
	 * 
	 * @param goal the Prolog string representing the goal to drop
	 */
	public boolean dropGoal(String goal) {
		return call(PrologKB.drop_goal, goal);
	}	
	
	public boolean isAdoptedGoal(String goal) {
		return call(PrologKB.adopted_goal, goal);
	}
	
	public boolean isAdoptedJustifiableGoal(String goal) {
		return call(PrologKB.adopted_justifiable_goal, goal);
	}	
	
	public boolean suggestGoal(String goal) {
		return call(PrologKB.suggest_goal, goal);
	}
	
	public boolean isSuggestedGoal(String goal) {
		return call(PrologKB.suggested_goal, goal);
	}
	


	/**
	 * Tell Prolog to adopt a justifiable goal. This makes it no longer eligible for goal selection
	 * 
	 * @param goal the Prolog string representing the goal to justify
	 */
	public boolean adoptJustifiableGoal(String goal) {
		return call(PrologKB.adopt_justifiable_goal, goal);
	}

	/**
	 * Applies operator effects
	 * 
	 * @param schema the operator schema to apply the effects of
	 *  
	 * @return wether successful
	 */
	public boolean applyOperatorEffects(String schema) {
		if (call(PrologKB.applyOperatorEffects, schema) ) {
			return true;
		} else {
			if (logger.isLoggable(Level.WARNING)) { 
				logger.warning("Applying operator effects failed.\nSchema: " + schema);
			}
			return false;
		}
	}

	/* See interface */
	public boolean ask(String query) {
		Query q = new Query(query);
		return q.hasSolution();
	}
	
	/* See interface */
	public boolean call(String prologCommand, String input) {
		Query query = new Query(prologCommand + "(" + input + ").");
		return query.hasSolution();
	}	
	

	/**
	 * Determine which actions given character can pursue
	 * @param character the URI of the character
	 * @return a vector of actions
	 */
	public Vector<String> canDo(String character, String operator) {
		return getPrologSingleVariableList(
				PrologKB.canDo, PrologKB.addQuotes(character) + "," + operator);				
	}

	
	/**
	 * Checks whether given schema's preconditions hold, and ignores fabula
	 * 
	 * @param schema the schema to validate, as prolog string
	 * @return whether the schemas preconditions (that are not fabula preconditions) are true 
	 */
	public boolean checkSchemaFacts(String schema) {
		
		StringBuilder queryString = new StringBuilder();
		queryString.append(PrologKB.check_schema_facts).append('(').append(schema).append(").");
		
		return ask(queryString.toString()); 

	}
	
	public Vector<RDFtriple> conditionToTripleList(String condition) {
		
		Vector<RDFtriple> triples = new Vector<RDFtriple>();
		
		// Build query
		StringBuilder sb = new StringBuilder();
		sb.append(condition_to_triples).append("(");
		sb.append(condition);
		sb.append(",T,S,P,O)");
		Query q = new Query(sb.toString());
		
		// Retrieve results
		Vector<RDFtriple> returnList = new Vector<RDFtriple>();
		for (Map<String, Term> binding: q.allSolutions()) {
			String t = binding.get("T").toString();
			String s = binding.get("S").toString();
			String p = binding.get("P").toString();
			String o = binding.get("O").toString();
			
			RDFtriple trip = new RDFtriple();
			trip.setSubject(removeQuotes(s));
			trip.setPredicate(removeQuotes(p));
			trip.setObject(removeQuotes(o));
			trip.setTruth(Boolean.parseBoolean(t));
			triples.add(trip);
		}

		q.close();
		return returnList;				

	}	

	/**
	 *  consult expects foreward slashes (/) in file names
	 **/
	public boolean consult(String filename) {
		Query query = new Query(PrologKB.consult + "('" + filename + "').");
		return query.hasSolution();
	}
	
	/**
	 * Returns the events in the plan that are executable, i.e. do not depend on the execution of other steps (in terms of causal links)
	 * 
	 * @param plan a prolog string representation of a plan
	 * @return a collection of prolog strings representing the schemas of steps in the plan
	 */
	public Vector<String> executableEvents(String plan) {
		if (logger.isLoggable(Level.FINE)) {
			logger.fine("Getting executable events for plan.");
		}
		return PrologKB.getInstance().getPrologSingleVariableList(
				PrologKB.executableEvent, plan);
	}	

	/**
	 * Returns the improvisations in the plan that are executable, i.e. do not depend on the execution of other steps (in terms of causal links)
	 * 
	 * @param plan a prolog string representation of a plan
	 * @return a collection of prolog strings representing the schemas of steps in the plan
	 */
	public Vector<String> executableFramingOperators(String plan) {
		if (logger.isLoggable(Level.FINE)) {
			logger.fine("Getting executable framing operators for plan.");
		}
		return PrologKB.getInstance().getPrologSingleVariableList(
				PrologKB.executableFramingOperator, plan);
	}
	
	/**
	 * Returns the inferences in the plan that are executable, i.e. do not depend on the execution of other steps (in terms of causal links)
	 * 
	 * @param plan a prolog string representation of a plan
	 * @return a collection of prolog strings representing the schemas of steps in the plan
	 */
	public Vector<String> executableInferenceOperators(String plan) {
		logger.fine("Getting executable inference operators for plan.");
		return PrologKB.getInstance().getPrologSingleVariableList(
				PrologKB.executableInferenceOperator, plan);
	}
	
	/**
	 * Returns the operators in the plan that are executable, i.e. do not depend on the execution of other steps (in terms of causal links)
	 * 
	 * @param plan a prolog string representation of a plan
	 * @return a collection of prolog strings representing the schemas of steps in the plan
	 */
	public Vector<String> executableOperators(String plan) {
		if (logger.isLoggable(Level.FINE)) {
			logger.fine("Getting executable operators for plan.");
		}
		return PrologKB.getInstance().getPrologSingleVariableList(
				PrologKB.executableOperator, plan);
	}

	
	/**
	 * Determines whether given plan is "finished", i.e., there are no more steps that can be executed
	 * @param plan Prolog string representing the plan
	 * @return whether plan is finished
	 */
	public boolean finishedPlan(String plan) {
		if (logger.isLoggable(Level.FINE)) {
			logger.fine("Seeing if plan is finished");
		}
		if (plan == null) {
			return false;
		}
		return call(finishedPlan, plan);
	}

	/**
	 * Determines whether plan is still valid in given context, i.e., replanning is not needed. This is a speedup for the planner.
	 * @param plan a Prolog string representing the plan

	 * @return whether the plan is "invalid"
	 */
	public boolean invalidPlan(String plan) {
		if (logger.isLoggable(Level.FINE)) {
			logger.fine("Seeing if current plan is invalid");
		}
		if (plan == null) {
			return true;
		}
		return call(invalidPlan, plan);
	}
	
	/**
	 * Get first element of tuple
	 * @param tuple a string representing the tuple
	 * @return first element of the tuple
	 */
	public String first(String tuple) {
		return getPrologSingleVariable(PrologKB.first, tuple);
	}
	
	
	/**
	 * Given a query and a variable occurring in this query, get all resulting binding from the answers
	 */
	public Vector<String> getAllResults(Query q, Variable v) {
		Vector<String> returnList = new Vector<String>();
		for (Map<String, Term> binding: q.allSolutions()) {
			Term t = (Term) binding.get(v.toString());
			if (t != null) {
				returnList.add(t.toString());
			}			
		}
		q.close();
		return returnList;
	}
	
/*---------------------------------
 * METHODS
 * --------------------------------
 */	
	
	/**
	 * This method returns a number of Strings that represent the URIs of Individuals of fabula elements, that 
	 * enable the given schema. For instance, if #belief_23 and #belief_25 make the preconditions of #goal_22 true,
	 * this method returns <#belief_23, #belief_25>. 
	 * @param gs the goal schema under investigation
	 * @return a set of individuals
	 */
	public Set<String> getEnablingFabulaElements(GoalSchema gs) {	return getEnablingFabulaElements(gs.getPrologDescription());	}	
	public Set<String> getEnablingFabulaElements(Operator op) {		return getEnablingFabulaElements(op.getPrologDescription());	}		
	protected Set<String> getEnablingFabulaElements(String schema) {
		StringBuilder sb = new StringBuilder();
		sb.append(schema_enablement).append("(");
		sb.append(schema);
		sb.append(",Individual)");
		Query q = new Query(sb.toString());
		
		// Retrieve results
		Set<String> returnList = new HashSet<String>();
		for (Map<String, Term> binding: q.allSolutions()) {
			String ind = binding.get("Individual").toString();
			
			returnList.add(removeQuotes(ind));
		}

		q.close();
		return returnList;		
	}
	
	public String getGoalPossibleAfterPlan(String character, String goal) {
		Atom charc = new Atom(character);
		return getPrologSingleVariable(PrologKB.possible_goal_after_plan, charc + "," + goal);
	}
	
	public Map<String,String> getGoalsPossibleAfterPlan(String character) {
		Map<String,String> goalPlanMap = new HashMap<String,String>();
		
		Variable G = new Variable("Goal");
		Variable P = new Variable("Plan");
		Atom charc = new Atom(character);
		// TODO use new Atom and new Term and new Query things
		StringBuilder sb = new StringBuilder();
		sb.append(PrologKB.possible_goal_after_plan).append('(').append(charc).append(", ").append(
				G.toString()).append(", ").append(P.toString()).append(").");
		
		Query q = new Query(sb.toString()); //prologCommand + "(" + input + ", " + X.toString() + ").");
		if (logger.isLoggable(Level.FINE)) {
			logger.fine("Querying prolog with: " + q.toString());
		}
		//Map<String, Term>[] answers = q.allSolutions();

		while (q.hasMoreElements()) {
			Map<String, Term> binding = (Map<String, Term>) q.nextElement();
			Term goal = (Term) binding.get(G.toString());
			Term plan = (Term) binding.get(P.toString());
			if (goal != null && plan != null) {
				goalPlanMap.put(goal.toString(), plan.toString());
			}
		}
		q.close();
		if (logger.isLoggable(Level.FINE)) {
			logger.fine("Prolog returned: " + goalPlanMap);
		}
		return goalPlanMap;					
	}	
	
//------------------------- 
//	Character agent module
//-------------------------
	
	public float getGoalUrgency(String schema) {
		String urg = getQNPF(PrologKB.getGoalUrgency, schema);
		return Float.parseFloat(urg);
	}
	
	/**
	 * This method returns a number of Strings that represent the URIs of Individuals of fabula elements, that 
	 * motivate the given schema. For instance, if #goal_23 and #goal_25 make the preconditions of #goal_22 true,
	 * this method returns <#goal_23, #goal_25>. 
	 * @param gs the Goal schema under investigation
	 * @return the set of Individuals as described
	 */
	public Set<String> getMotivatingFabulaElements(GoalSchema gs) {
		StringBuilder sb = new StringBuilder();
		sb.append(goal_motivation).append("(");
		sb.append(gs.getPrologDescription());
		sb.append(",Individual)");
		Query q = new Query(sb.toString());
		
		// Retrieve results
		Set<String> returnList = new HashSet<String>();
		for (Map<String, Term> binding: q.allSolutions()) {
			String ind = binding.get("Individual").toString();
			
			returnList.add(removeQuotes(ind));
		}

		q.close();
		return returnList;		
	}
	
	/**
	 * Given a query and a variable occurring in this query, get one resulting binding from the answers
	 */
	public String getOneResult(Query q, Variable v) {
		Map<String, Term> binding = q.oneSolution();
		q.close();
		if (binding != null) {
			Term t = (Term) binding.get(v.toString());
			if (t != null) {
				return t.toString();
			}			
		}
		return null;
	}	
	
	/**
	 * Retrieve effects of operator schema
	 * assumption is that effects can be applied 
	 * 
	 * @param schema the schema to retrieve the effects of, in prolog string
	 * @return a vector of the retrieved triples, or null if applying failed
	 */
	public Vector<RDFtriple> getOperatorEffects(String schema) {
		if (logger.isLoggable(Level.FINE)) {
			logger.fine("Successfully applied operator effects");
		}
		Vector<RDFtriple> effects = new Vector<RDFtriple>();
		
		// Build query
		StringBuilder sb = new StringBuilder();
		sb.append(operator_effect).append("(");
		sb.append(schema);
		sb.append(",T,S,P,O)");
		Query q = new Query(sb.toString());
		
		// Retrieve results
		Vector<RDFtriple> returnList = new Vector<RDFtriple>();
		for (Map<String, Term> binding: q.allSolutions()) {
			String t = binding.get("T").toString();
			String s = binding.get("S").toString();
			String p = binding.get("P").toString();
			String o = binding.get("O").toString();
			
			RDFtriple trip = new RDFtriple();
			trip.setSubject(removeQuotes(s));
			trip.setPredicate(removeQuotes(p));
			trip.setObject(removeQuotes(o));
			trip.setTruth(Boolean.parseBoolean(t));
			effects.add(trip);
		}

		q.close();
		return effects;
	}
	
	/**
	 * Returns a collection of the plan links of given plan 
	 * @param plan a prolog string representation of a plan
	 * @return a collection of plan links
	 */
	public Vector<PlanLink> getPlanLinks(String plan) {
		if (logger.isLoggable(Level.FINE)) {
			logger.fine("Retrieving all Links from Plan.");
		}
		Vector<PlanLink> planLinks = new Vector<PlanLink>();
		
		Vector<String> links = PrologKB.getInstance().getPrologSingleVariableList(
				PrologKB.planLink, plan);
		
		for (String link: links) {
			String from = PrologKB.getInstance().getPrologSingleVariable(PrologKB.planLinkFrom, link);
			String to = PrologKB.getInstance().getPrologSingleVariable(PrologKB.planLinkTo, link);
			String cond = PrologKB.getInstance().getPrologSingleVariable(PrologKB.planLinkCond, link);

			//RDFtriple posTriple = PrologKB.getInstance().prologToTriple(pos, true);
			//RDFtriple negTriple = PrologKB.getInstance().prologToTriple(neg, false);
			
			PlanLink nwLink = new PlanLink(from, to, cond);
			planLinks.add(nwLink);
		}

		return planLinks;
		
	}
	
	/**
	 * Returns a collection of the plan orderings of given plan 
	 * @param plan a prolog string representation of a plan
	 * @return a collection of plan orderings
	 */
	public Vector<PlanOrdering> getPlanOrderings(String plan) {
		if (logger.isLoggable(Level.FINE)) {
			logger.fine("Retrieving all Links from Plan.");
		}
		Vector<String> orderings = PrologKB.getInstance().getPrologSingleVariableList(
				PrologKB.planOrdering, plan);
		
		Vector<PlanOrdering> planOrderings = new Vector<PlanOrdering>();
		
		
		for (String ordering: orderings) {
			
			String v1 = PrologKB.getInstance().first(ordering);
			String v2 = PrologKB.getInstance().second(ordering);
			
			String v1_name = PrologKB.getInstance().first(v1);
			String v2_name = PrologKB.getInstance().first(v2);
			
			PlanOrdering nwOrdering = new PlanOrdering(v1_name, v2_name);
			planOrderings.add(nwOrdering);
		}

		return planOrderings;
		
	}
	
	public Vector<PlanStep> getPlanSteps(String plan) {
		Vector<PlanStep> planSteps = new Vector<PlanStep>();
		Vector<String> steps = PrologKB.getInstance().getPrologSingleVariableList(
				PrologKB.planStep, plan);
		
		for (String step: steps) {
			String name = PrologKB.getInstance().first(step);
			String operator = PrologKB.getInstance().second(step);
			String type = PrologKB.getInstance().getSchemaType(operator);
			String clss = PrologKB.getInstance().getSchemaClass(operator);
			
			PlanStep nwStep = new PlanStep(name, operator, type, clss);
			planSteps.add(nwStep);
		}
		
		return planSteps;

	}	
	
	/**
	 * Returns a vector containing all goals that are possible to pursue for given character. In BDI terms, these are the "desires"
	 * of the agent. Prolog is responsible for establishing this set of desires; Java is responsible for selecting what to pursue.
	 * No choices are made yet as to a consistent set of goals that the agent is actually pursuing (in BDI terms, the "goals" of the agent). 
	 * 
	 * @param character the URI of the character agent for which to retrieve possible goals
	 * @return a list of possible goals
	 */
	public Vector<String> getPossibleGoals(String character) {
		return getPrologSingleVariableList(
				PrologKB.possible_goal, PrologKB.addQuotes(character));						
	}
	
	/**
	 * Returns a collection of settings based on executed plot threads (asserted in Prolog)
	 * @return a collection of (thread) settings as Prolog strings
	 */
	public Vector<String> selectReactiveActions(String character) {
		return getPrologSingleVariableList(PrologKB.select_action_reactively, PrologKB.addQuotes(character));
	}
	
	
	/**
	 * Builds the following structure
	 * <i>prologCommand</i> ( <i>input</i> , GPLVar )
	 * @deprecated - use getOneResult() in future
	 */
	public String getPrologSingleVariable(String prologCommand, String input) {
		Variable X = new Variable("GPLVar");
		// TODO use new Atom and new Term and new Query things
		StringBuilder sb = new StringBuilder();
		sb.append(prologCommand).append('(').append(input).append(", ").append(
				X.toString()).append(").");
		Query q = new Query(sb.toString());//prologCommand + "(" + input + ", " + X.toString() + ").");
		if (logger.isLoggable(Level.FINE)) {
			logger.fine("Querying prolog with: " + q.toString());
		}
		String answer = new String();
		//TODO This is ugly
		if (q.hasMoreElements()) {
			Map<String, Term> binding = (Map<String, Term>) q.nextElement();
			Term t = (Term) binding.get(X.toString());
			if (t != null) {
				answer = t.toString();
			}
		}
		q.close();
		if (logger.isLoggable(Level.FINE)) {
			logger.fine("Prolog returned: " + answer);
		}
		return answer;
	}
	
//------------------------- 
//	Knowledge base module
//-------------------------	
	
	@Deprecated
	public Vector<String> getPrologSingleVariableList(String prologCommand) {
		Variable X = new Variable("GPLVar");
		// TODO use new Atom and new Term and new Query things
		Query q = new Query(prologCommand + "(" + X.toString() + ").");
		
		if (logger.isLoggable(Level.FINE)) {
			logger.fine("Querying prolog with: " + q.toString());
		}
		
		//Map<String, Term>[] answers = q.allSolutions();
		Vector<String> returnList = new Vector<String>();
		while (q.hasMoreElements()) {
			Map<String, Term> binding = (Map<String, Term>) q.nextElement();
			Term t = (Term) binding.get(X.toString());
			if (t != null) {
				returnList.add(t.toString());
			}
		}
		q.close();
		if (logger.isLoggable(Level.FINE)) {
			logger.fine("Prolog returned: " + returnList);
		}
		return returnList;
	}

	/**
	 * @deprecated - use getAllResults() in future
	 */
	public Vector<String> getPrologSingleVariableList(String prologCommand,	String input) {
		Variable X = new Variable("GPLVar");
		// TODO use new Atom and new Term and new Query things
		StringBuilder sb = new StringBuilder();
		sb.append(prologCommand).append('(').append(input).append(", ").append(
				X.toString()).append(").");
		Query q = new Query(sb.toString()); //prologCommand + "(" + input + ", " + X.toString() + ").");
		logger.fine("Querying prolog with: " + q.toString());
		//Map<String, Term>[] answers = q.allSolutions();
		Vector<String> returnList = new Vector<String>();
		while (q.hasMoreElements()) {
			Map<String, Term> binding = (Map<String, Term>) q.nextElement();
			Term t = (Term) binding.get(X.toString());
			if (t != null) {
				returnList.add(t.toString());
			}
		}
		q.close();
		
		if (logger.isLoggable(Level.FINE)) {
			logger.fine("Prolog returned: " + returnList);
		}
		return returnList;
	}	
	
//---------------------------
//	Schema management module
//---------------------------

	
	// Returns the answer that Prolog returns without quotes and if the answer
	// is "none" or "" the result is set to null
	public String getQNPF(String command, String action) {
		String output = removeQuotes(
				getPrologSingleVariable(command, action));
		if (output != null) {
			if (output.contentEquals("none") || output.contentEquals("")) {
				output = null;
			}
		}
		return output;
	}
	/**
	 * Returns the URI of the agens of given schema (if it has any) 
	 * @param schema the schema as prolog string
	 * @return the agens of the schema
	 */
	public String getSchemaAgens(String schema) {
		return getQNPF(PrologKB.getOperatorAgens, schema);
	}
	/**
	 * Returns the class of given schema (e.g. action, goal, event, etc)
	 * 
	 * TODO: make in prolog using =..
	 * 
	 * @param schema the schema as Prolog string
	 * @return the class of the schema
	 */
	public String getSchemaClass(String schema) {
		return getQNPF(PrologKB.getSchemaKind, schema);		
	}
	
	/**
	 * Returns the URI of the instrument of given schema (if it has any) 
	 * @param schema the schema as prolog string
	 * @return the instrument of the schema
	 */
	public String getSchemaInstrument(String schema) {
		return getQNPF(PrologKB.getOperatorInstrument, schema);
	}
	
	/**
	 * Returns the URI of the opponent of given schema (if it has any) 
	 * @param schema the schema as prolog string
	 * @return the instrument of the schema
	 */
	public String getSchemaOpponent(String schema) {
		return getQNPF(PrologKB.getOperatorOpponent, schema);
	}		
	
	public int getOperatorDuration(String operator) {
		return Integer.parseInt(getQNPF(PrologKB.getOperatorDuration, operator));
	}
	
	/**
	 * Returns the URI of the patiens of given schema (if it has any) 
	 * @param schema the schema as prolog string
	 * @return the patiens of the schema
	 */
	public String getSchemaPatiens(String schema) {
		return getQNPF(PrologKB.getOperatorPatiens, schema);
	}
	
	
	/**
	 * Retrieves preconditions of given schema
	 * @param schema the schema to retrieve the preconditions of
	 * @return Prolog string representing the preconditions
	 */
	public String getSchemaPreconditions(String schema) {

		return getPrologSingleVariable(
				PrologKB.getSchemaPreconditions, schema);
		
	}
	
	/**
	 * Returns the URI of the target of given schema (if it has any) 
	 * @param schema the schema as prolog string
	 * @return the target of the schema
	 */
	public String getSchemaTarget(String schema) {
		return getQNPF(PrologKB.getOperatorTarget, schema);
	}	
	
	/**
	 * Returns the type of given schema (its corresponding URI in the ontology)
	 * @param schema the schema as Prolog string
	 * @return the type of the schema
	 */
	public String getSchemaType(String schema) {
		return getQNPF(PrologKB.getOperatorType, schema);
	}
	
	public boolean isFramingScopeAll(String schema) {
		boolean sa = call(getFramingScopeAll, schema);
		if (sa) {
			if (logger.isLoggable(Level.FINE)) {
				logger.fine("Given schema has scope ALL");
			}
		}
		return sa;
	}
	
	public boolean isFramingScopePersonal(String schema) {
		boolean sp = call(getFramingScopePersonal, schema);
		if (sp) {
			if (logger.isLoggable(Level.FINE)) {
				logger.fine("Given schema has scope PERSONAL");
			}
		}
		return sp;
	}	
	
	public boolean isFramingScopeHidden(String schema) {
		boolean sh = call(getFramingScopeHidden, schema);
		if (sh) {
			if (logger.isLoggable(Level.FINE)) {
				logger.fine("Given schema has scope HIDDEN");
			}
		}
		return sh;
	}	
	
	public String getUntruePreconditionsOfSchema(String schema) {
		return getPrologSingleVariable(PrologKB.validateSchema, schema);
	}
	
	/**
	 * Checks whether given goal's failure conditions are true
	 * @param schema the goal schema
	 * @return whether failure conditions of goal schema are true
	 */
	public boolean goalFailureConditionsTrue(String schema) {
		return call(validateGoalFailure, schema);
	}
	
	public String goalIntention(String goalSchema) {

		return getPrologSingleVariable(
				PrologKB.goal_intention, goalSchema);
		
	}
	
	/**
	 * Determine which actions given character can pursue
	 * @param character the URI of the character
	 * @return a vector of actions
	 */
	public Vector<String> hasAction(String character) {
		return getPrologSingleVariableList(
				PrologKB.hasAction,PrologKB.addQuotes(character));				
	}	

	/**
	 * Load RDF/OWL knowledge
	 * @param file filename of the knowledge base
	 * @return whether loading succeeded
	 */
	public boolean loadKB(String file) {
		// TODO: translate to stringbuilder (JPL has no support for modules so this does not work)
		Compound c = new Compound(PrologKB.rdf_load, new Term[] {new Atom(file)});
		Query query = new Query(c);
		return query.hasSolution();
	}
	
	/**
	 * Make a narration of given prolog string
	 * @param prologString prolog string representing a narratable structure.
	 * @return narration of this prolog string in "natural language"
	 */
	public String narrate(String prologString) {
		return getPrologSingleVariable(PrologKB.narrate, prologString);
	}		
	
	/**
	 * Retrieves characters necessary as a result of plot thread executions (asserted in Prolog)
	 * @return a vector containing URIs of characters that are necessary 
	 */	
	public Vector<String> necessaryCharacters(String thread) {
		return getPrologSingleVariableList(PrologKB.necessaryCharacter, thread);
	}
	
	/**
	 * Builds and retrieves a partial-order plan
	 * @param intentions the intentions as prolog list
	 * @return a plan as prolog String, or null if there was no solution.
	 */
	public String plan(String character, String intentions) {
		String charField; // no pun intended
		
		if (character != null && (! character.equals(""))) {
			Atom a = new Atom(character);
			charField = a.toString();
		} else {
			charField = "_";
		}
		StringBuilder sb = new StringBuilder();		
		sb.append(PrologKB.plan).append('(')
			.append(charField).append(',')
			.append(intentions).append(", Plan).");
		
		
		if (logger.isLoggable(Level.INFO)) {
			logger.info ("Querying plan with: " + sb.toString ());
		}
		
		Query q = new Query(sb.toString());
		String solution = PrologKB.getInstance().getOneResult(q, new Variable("Plan"));
		//Map<String, Term> solution = PrologKB.getInstance().prologCallOneSolution(sb.toString());

		if (logger.isLoggable(Level.INFO)) {
			if (solution != null) {
				logger.info("Successfully created plan using\n" + sb.toString());
			} else {
				logger.info("Could not create plan using\n" + sb.toString());
			}
		}
		
		return solution;
		
	}
	
	/**
	 * Adapts an existing partial-order plan
	 * @param intentions the intentions as prolog list
	 * @return a plan as prolog String, or null if there was no solution.
	 */
	public String adaptPlan(String character, String intentions, String oldPlan) {
		
		// Speedup: only adapt plan if current plan is no longer valid.
		// might yield believability issues because "obvious" plan adaptions will not be chosen
		// (i.e. still sailing to the island to find a treasure chest 
		//  even though someone has placed another treasure chest right in front of your nose.)
		if (! invalidPlan(oldPlan)) {
			if (logger.isLoggable(Level.INFO)) {
				logger.info("Old plan still OK, reusing it.");
			}
			return oldPlan;
		}
		
		// TODO: if old plan is no longer valid, according to Trabasso et al. what should happen is that 
		// the goal gets a negative outcome (O-), and the goal is re-instantiated (if its preconditions still hold). 
		
		String charField; // no pun intended
		
		if (character != null && (! character.equals(""))) {
			Atom a = new Atom(character);
			charField = a.toString();
		} else {
			charField = "_";
		}
		StringBuilder sb = new StringBuilder();		
		sb.append(PrologKB.adaptPlan).append('(')
			.append(charField).append(',')			
			.append(intentions).append(',')
			.append(oldPlan).append(", Plan).");
		
		
		if (logger.isLoggable(Level.INFO)) {
			logger.info ("Querying adapt_plan with: " + sb.toString ());
		}
		
		Query q = new Query(sb.toString());
		String solution = PrologKB.getInstance().getOneResult(q, new Variable("Plan"));
		//Map<String, Term> solution = PrologKB.getInstance().prologCallOneSolution(sb.toString());

		if (logger.isLoggable(Level.INFO)) {
			if (solution != null) {
				logger.info("Successfully created plan using\n" + sb.toString());
			} else {
				logger.info("Could not create plan using\n" + sb.toString());
			}			
		}
		
		return solution;
		
	}	
	
//--------------------------------
//	Partial order planning module
//--------------------------------
	
	/**
	 * Retrieve plot threads that are now possible
	 * 
	 * @return vector containing string representations of possible plot threads
	 */
	public Vector<String> possibleThreads() {
		return PrologKB.getInstance().getPrologSingleVariableList(PrologKB.possibleThread);		
	}
	
	public Map<String, Term>[] prologCall(String prologString) {
		Query q = new Query(prologString);
		Map<String, Term>[] answers = q.allSolutions();
		q.close();
		return answers;
	}
	
	public Map<String, Term>[] prologCall(String prologCommand, String input,
			Vector<String> vars) {
		// Use prologCommand^(AgentID, Variable) which is in prolog in BasicCharacterAgent.pl
		StringBuilder queryString = new StringBuilder();
		queryString.append(prologCommand).append('(').append(input);

		for (String v : vars) {
			queryString.append(',').append(v);
		}
		queryString.append(").");

		Query q = new Query(queryString.toString()); //prologCommand + "(" + input + queryString.toString() + ").");
		if (logger.isLoggable(Level.INFO)) {
			logger.info("Querying Prolog with: " + queryString.toString());//prologCommand + "(" + input + queryString.toString() + ").");
		}
		Map<String, Term>[] answers = q.allSolutions();
		q.close();
		return answers;
	}
	
	public Map<String, Term> prologCallOneSolution(String prologString) {
		Query q = new Query(prologString);
		Map<String, Term> answer = q.oneSolution();
		q.close();
		return answer;
	}
	
	/*
	 * @see vs.knowledge.IPrologKnowledgeManager#prologToTriple(java.lang.String, boolean)
	 */
	public List<RDFtriple> prologToList(String prologTripleList, boolean truth) {
		
		List<RDFtriple> tripleList = new Vector<RDFtriple>();
		Term[] triples = null;
		
		try {
			Term t = Util.textToTerm(prologTripleList);
			triples = Util.listToTermArray(t);
		} catch(JPLException e) {
			//do nothing
			if (logger.isLoggable(Level.WARNING)) {
				logger.warning("Could not create triples from given Prolog list string: " + prologTripleList);
			}
		} 
		
		for (Term t: triples) {
			tripleList.add(prologToTriple(t.toString(), truth));
		}
		
		return tripleList;
	}
	
	
	/**
	 * Builds an RDFtriple object from prolog string representation of an rdf triple
	 * TODO: replace getSubject...etc by JPL parsing
	 * 
	 * @param prologTriple the string representation of the triple
	 * @param truth whether the triple should be interpreted as true or false
	 * @return an RDFtriple object if successful, or null if unsuccessful
	 */
	public RDFtriple prologToTriple(String prologTriple, boolean truth) {
		RDFtriple t = new RDFtriple();
		
		Term term = Util.textToTerm(prologTriple);
		String subject = getPrologSingleVariable(PrologKB.getSubject,
				prologTriple);
		String predicate = getPrologSingleVariable(PrologKB.getPredicate,
				prologTriple);
		String object = getPrologSingleVariable(PrologKB.getObject,
				prologTriple);
		
		if (subject.equals("") || predicate.equals("") || object.equals("")) {
			return null;
		} else {
			t.setSubject(removeQuotes(subject));
			t.setPredicate(removeQuotes(predicate));
			t.setObject(removeQuotes(object));
			t.setTruth(truth);
		}
		
		return t;
	}
	
	/**
	 * RDF query method
	 * @param triple the RDF triple we want to query Prolog about.
	 * @return a Query object
	 */
	public Query query(String triple) {
		return new Query(PrologKB.query + "(" + triple + ")");
	}
	
	/**
	 * Save knowledge base. 
	 * Not implemented yet.
	 * @param file file name to save KB to
	 * @return whether saving succeeded
	 */
	public boolean saveKB(String file) {
		// TODO Auto-generated method stub
		return false;
	}	
	
//--------------------------------
//	Thread management module
//--------------------------------

	/**
	 * Get second element of tuple
	 * @param tuple a string representing the tuple
	 * @return second element of the tuple
	 */
	public String second(String tuple) {
		return getPrologSingleVariable(PrologKB.second, tuple);
	}
	

	/**
	 * Set the character ID for this agent (e.g. ps:leChuck)
	 */
	public boolean setAgentID(String agentID) {
		if (logger.isLoggable(Level.INFO)) {
			logger.info("Setting agent ID to: " + agentID);
		}

		StringBuilder qb = new StringBuilder();
		Atom a = new Atom(agentID);
		qb.append(PrologKB.setAgentID).append('(').append(a.toString()).append(')');

		Query q = new Query(qb.toString());
		return q.hasSolution();
	}
	
/*	*//*
	 * Assert that given character is now casted
	 * @param character the character that has been casted
	 * @return whether asserting succeeded
	 *//*
	public boolean castedCharacter(String character) {
		return call(PrologKB.castedCharacter, character);
	}*/
	
	/* See interface */
	public boolean tellRDF(String term) {
		Query q = new Query(PrologKB.rdfAssert + "(" + term + ")");
		//new Query("assert(" + term + ")");
		return q.hasSolution();
	}
	
	public boolean tellRDFtriple(RDFtriple t) {

		if (t.getTruth() == true) {
			if (logger.isLoggable(Level.FINE)) {
				logger.fine("Asserting RDF triple: " + t);
			}
			Query q = new Query(PrologKB.rdfAssert + "(" + tripleToProlog(t) + ")");
			return q.hasSolution();
		} else {
			if (logger.isLoggable(Level.FINE)) {
				logger.fine("Retracting RDF triple: " + t);
			}
			Query q = new Query(PrologKB.rdfRetract + "(" + tripleToProlog(t) + ")");
			return q.hasSolution();
		}
	}
	
	/**
	 * Returns a collection of plot thread goals for given character
	 * 
	 * @param characterURI the URI of the character to retrieve plot thread goals for
	 * @return a collection of goals that the character should adopt
	 */
	public Vector<String> threadGoals(String episode, String characterURI) {
		return getPrologSingleVariableList(PrologKB.threadGoal, episode + "," + PrologKB.addQuotes(characterURI));
	}
	
	/**
	 * Returns a collection of plot thread resolve goals for given character
	 * 
	 * @param characterURI the URI of the character to retrieve plot thread goals for
	 * @return a collection of goals that the character should adopt
	 */
	public Vector<String> threadResolveGoals(String episode, String characterURI) {
		return getPrologSingleVariableList(PrologKB.threadResolveGoal, episode + "," + PrologKB.addQuotes(characterURI));
	}
	
//--------------------------------
//	Narrator module
//--------------------------------	
	
	/**
	 * Returns a collection of settings based on executed plot threads (asserted in Prolog)
	 * @return a collection of (thread) settings as Prolog strings
	 */
	public Vector<String> threadSettings(String thread) {
		return getPrologSingleVariableList(PrologKB.threadSetting, thread);
	}
		
	
/*=====================================================================================================================
 * HELPER METHODS
/*=====================================================================================================================
 */	

	/**
	 * Returns true iff left hand side unifies with right hand side
	 * @param lhs left hand side of the unification
	 * @param rhs right hand side of the unification
	 * @return whether unification succeeds
	 */
	public boolean unifies(String lhs, String rhs) {		
		Query q = new Query(lhs + " = " + rhs);
		if (q.hasSolution()) {
			if (logger.isLoggable(Level.FINE)) {
				logger.fine("Checking whether following two terms unify:\n1) " + lhs + "\n2) " + rhs + "\nThey do.");
			}
			return true;
		} else {
			if (logger.isLoggable(Level.FINE)) {
				logger.fine("Checking whether following two terms unify:\n1) " + lhs + "\n2) " + rhs + "\nThey do NOT.");
			}
			return false;
		}
		
	}
	
	/* See interface */
	public boolean untellRDF(String term) {
		Query q = new Query(PrologKB.rdfRetract + "(" + term + ")");
		//new Query("retract(" + term + ")");
		return q.hasSolution();
	}
	
}
