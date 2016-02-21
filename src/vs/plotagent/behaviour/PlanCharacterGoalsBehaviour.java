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
package vs.plotagent.behaviour;

import jade.core.behaviours.SimpleBehaviour;

import java.util.Set;
import java.util.Vector;
import java.util.logging.Logger;

import vs.debug.LogFactory;
import vs.knowledge.PrologKB;
import vs.knowledge.vocab.Fabula;
import vs.plotagent.BasicPlotAgent;
import vs.plotagent.inspiration.Suggestion;
import vs.rationalagent.RationalAgent;

public class PlanCharacterGoalsBehaviour extends SimpleBehaviour {

	BasicPlotAgent plotAgent;
	
	public PlanCharacterGoalsBehaviour(BasicPlotAgent arg0) {
		super(arg0.getAgent());
		plotAgent = arg0;
	}

	@Override
	public void action() {
		Logger logger = LogFactory.getLogger(this);
		// TODO Auto-generated method stub
		logger.info("Planning character goals...");
		((RationalAgent)myAgent ).writeConsole("Starting behaviour: plan character goals");
		
		Set<Suggestion> suggestions = plotAgent.getInspirationModule().getSuggestions();
		logger.info("Found " + suggestions.size() + " suggestions, checking which are goals...");
		
		// Walk through all suggestions
		for (Suggestion suggestion : suggestions) {
			logger.fine("...checking " + suggestion + "...");
			
			// Check for goal nodes
			Vector<String> goalNodes = plotAgent.getKnowledgeManager()
				.getPrologSingleVariableList(
						PrologKB.nodeClass,
						suggestion.getBody() + ", '" + Fabula.Goal + "'");
			
			for (String n : goalNodes) {

				logger.info("Goal contents of goal " + n + ":\n" 
						+ plotAgent.getKnowledgeManager().getPrologSingleVariableList(
								PrologKB.getFabulaContents, 
								suggestion.getBody() + "," + n));
				
			}			

		}

	}

	@Override
	public boolean done() {
		// TODO Auto-generated method stub
		return true;
	}

}
