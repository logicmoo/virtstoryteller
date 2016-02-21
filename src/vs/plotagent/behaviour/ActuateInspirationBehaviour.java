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

import vs.communication.FabulaCausality;
import vs.communication.Operator;
import vs.communication.StoryEvent;
import vs.debug.LogFactory;
import vs.knowledge.PrologKB;
import vs.plotagent.BasicPlotAgent;
import vs.plotagent.inspiration.OperationalizedSuggestion;
import vs.plotagent.inspiration.Suggestion;
import vs.plotagent.ui.NewSuggestionsCreatedEvent;

public class ActuateInspirationBehaviour extends SimpleBehaviour {

	BasicPlotAgent plotAgent;
	Logger logger;
	
	public ActuateInspirationBehaviour(BasicPlotAgent arg0) {
		super(arg0.getAgent());
		logger = LogFactory.getLogger(this);
		plotAgent = arg0;
	}

	@Override
	public void action() {
		Logger logger = LogFactory.getLogger(this);
		
		plotAgent.writeConsole("Starting behaviour: narrative inspiration");
		
		// Retrieve suggestions
		Set<Suggestion> rawSuggestions = plotAgent.getInspirationModule().getSuggestions();
		
		// Operationalize suggestions
		Set<OperationalizedSuggestion> operSuggestions = plotAgent.getInspirationModule().operationalizeSuggestions(rawSuggestions);
		plotAgent.fireEvent(new NewSuggestionsCreatedEvent(this, operSuggestions));
				
		// Choose a random event
		Vector<OperationalizedSuggestion> eventSuggestions = new Vector<OperationalizedSuggestion>();
		for (OperationalizedSuggestion os: operSuggestions) {
			if (os.getOperationalizedElement() instanceof StoryEvent) {
				logger.info("Story event");
				eventSuggestions.add(os);
			} else {
				logger.info("No story event, but: " + os.getOperationalizedElement().getClass());
			}
		}
		
		if (eventSuggestions.size() > 0) {
			int idx = (int) (eventSuggestions.size() * Math.random());
			logger.info("Out of " + eventSuggestions.size() + " event suggestions, picking #" + idx);
			executeSuggestion(eventSuggestions.elementAt(idx));
		} else {
			logger.info("No event suggestions.");
		}

	}
	
	@Override
	public boolean done() {
		// TODO Auto-generated method stub
		return true;
	}

	/**
	 * Executes a suggestion
	 * @param os the operationalized suggestion
	 */
	private void executeSuggestion(OperationalizedSuggestion os) {

		// Execute if operator
		if (os.getOperationalizedElement() instanceof Operator) {
			plotAgent.addBehaviour(new InitiateRequestPerformOperatorBehaviour(plotAgent,
				(Operator)os.getOperationalizedElement()));
		}

		// Mark suggestion as used
		plotAgent.getInspirationModule().addUsedSuggestion(os.getSuggestion());
		
		// Log to fabula
		plotAgent.getFabulaBuilder().addFabulaElement(os.getOperationalizedElement());
		logger.info("Sending fabula element: " + os.getOperationalizedElement());
		
		for (String causer: os.getSuggestion().getCausers()) {
			// Create causalities
			
			FabulaCausality fc = new FabulaCausality();			
			fc.setSubjectIndividual(PrologKB.removeQuotes(causer));
			fc.setObjectIndividual(PrologKB.removeQuotes(os.getOperationalizedElement().getIndividual()));
			String fabulaCausality = plotAgent.getKnowledgeManager()
					.getPrologSingleVariable(PrologKB.causalityClass,
							os.getSuggestion().getBody() + "," + 
							causer + "," + 
							os.getSuggestion().getIndividual());
			
			fc.setCausalProperty(PrologKB.removeQuotes(fabulaCausality));
			logger.info("Sending fabula causality: (" + fc.getSubjectIndividual() + "," + fc.getCausalProperty() + "," + fc.getObjectIndividual() + ")");
			plotAgent.getFabulaBuilder().addFabulaCausality(fc);
		}
	}

}
