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

import java.util.Set;

import vs.IAgentModule;
import vs.plotagent.inspiration.InspirationRule;
import vs.plotagent.inspiration.OperationalizedSuggestion;
import vs.plotagent.inspiration.Suggestion;

/**
 * The inspiration module is an implementation of the Narrative Inspiration idea.
 * It should deliver suggestions based on given fabula and rule base.
 * 
 * @author swartjes
 *
 */
public interface IInspirationModule extends IAgentModule {
	
	/**
	 * Mark a suggestion as having been used (the module should decide whether that means
	 * not to use it again)
	 * 
	 */
	public void addUsedSuggestion(Suggestion s);
	
	/**
	 * Generates a file containing the rules, generated from a fabula case
	 * 
	 * @param caseFileURL the input file containing the fabula
	 * 
	 * @return the generated rules
	 */
	public Set<InspirationRule> generateRules(String caseFileURL);
	
	/**
	 * Retrieves a set containing suggestions given the current state of the fabula
	 * 
	 * @return the set of suggestions
	 */
	public Set<Suggestion> getSuggestions();
	
	/**
	 * Loads a rule file into memory
	 * 
	 * @param filename the name of the rule file
	 */
	public void loadRules(String filename);
	
	/**
	 * Operationalizes given suggestions, meaning: make them ready for use.
	 * Creates OperationalizedSuggestions that contain:
	 * 	- information about which fabula elements caused the fabula element of this suggestion
	 *  - information about the type of the suggested fabula element
	 *  - the vs.communication object representing the operationalized suggestion   
	 * 
	 * @param suggestions the suggestions to operationalize
	 * @return the operationalized suggestions
	 */
	public Set<OperationalizedSuggestion> operationalizeSuggestions(Set<Suggestion> suggestions);
	
	/**
	 * Generates a file containing the given rules.
	 * 
	 * @param rules the rules to output
	 * @param ruleFileURL the output file containing the rules
	 */
	public void writeRules(Set<InspirationRule> rules, String ruleFileURL);	

}
