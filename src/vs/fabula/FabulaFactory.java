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


import jade.util.leap.List;
import vs.communication.GoalSchema;
import vs.communication.StoryAction;
import vs.communication.StoryBelief;
import vs.communication.StoryEvent;
import vs.communication.StoryOutcome;
import vs.communication.StoryPerception;
import vs.communication.StorySettingElement;
import vs.knowledge.PrologKB;
import vs.knowledge.vocab.Fabula;
import vs.rationalagent.StoryTime;
import vs.utils.UniqueId;

/**
 * Factory class for the construction of fabula elements. Deals with filling in individual name and OWL class
 * of the fabula elements, so that this happens in a unified and centralized place. 
 *  
 * @author swartjes
 */
public class FabulaFactory {

	public static enum Outcome {positive, negative, neutral};
	
	/**
	 * Create Action fabula element
	 * @param prologString the description of the action in Prolog, i.e. the head of its instantiated schema
	 * @param characterURI character of the action
	 * @return a new StoryAction
	 */
	public static StoryAction createAction(String prologString, String characterURI) {
		
		// Construct the action to do.
		StoryAction a = new StoryAction();
		a.setCharacter(characterURI);
		a.setPrologDescription(prologString);
		a.setIndividual(UniqueId.generateUniqueIndividual("Action",
				PrologKB.fromNrSign(characterURI)));
		a.setType(PrologKB.getInstance().getSchemaType(prologString));
		a.setAgens(PrologKB.getInstance().getSchemaAgens(prologString));
		a.setPatiens(PrologKB.getInstance().getSchemaPatiens(prologString));
		a.setTarget(PrologKB.getInstance().getSchemaTarget(prologString));
		a.setInstrument(PrologKB.getInstance().getSchemaInstrument(prologString));
		
		return a;
	}
	
	/**
	 * Create Event fabula element
	 * @param prologString the description of the action in Prolog, i.e. the head of its instantiated schema
	 * @param characterURI character of the event
	 * @return a new StoryAction
	 */
	public static StoryEvent createEvent(String prologString, String characterURI) {
		
		StoryEvent e = new StoryEvent();
		
		// Below info is not really needed for Events but might be useful for debugging purposes.
		e.setCharacter(characterURI); 
		e.setType(PrologKB.getInstance().getSchemaType(prologString));
		e.setIndividual(UniqueId.generateUniqueIndividual("Event",
				PrologKB.fromNrSign(characterURI)));
		e.setPrologDescription(prologString);
		e.setAgens(PrologKB.getInstance().getSchemaAgens(prologString));
		e.setPatiens(PrologKB.getInstance().getSchemaPatiens(prologString));
		e.setTarget(PrologKB.getInstance().getSchemaTarget(prologString));
		e.setInstrument(PrologKB.getInstance().getSchemaInstrument(prologString));
		
		return e;
	}
	
	/**
	 * Create Goal fabula element
	 * 
	 * @param prologString prolog representation of goal schema
	 * @param characterURI character of this Goal
	 * 
	 * @return a new StoryBelief
	 */
	public static GoalSchema createGoalSchema(String prologString, String characterURI) {
		GoalSchema gs = new GoalSchema();
		gs.setPrologDescription(prologString);
		gs.setType(PrologKB.getInstance().getSchemaType(prologString));
		gs.setCharacter(characterURI);
		gs.setIndividual(UniqueId.generateUniqueIndividual("Goal",
				PrologKB.fromNrSign(characterURI)));
		
		gs.setAgens(PrologKB.getInstance().getSchemaAgens(prologString));
		gs.setPatiens(PrologKB.getInstance().getSchemaPatiens(prologString));
		gs.setTarget(PrologKB.getInstance().getSchemaTarget(prologString));
		gs.setInstrument(PrologKB.getInstance().getSchemaInstrument(prologString));
		gs.setTime(StoryTime.getTime());
		return gs;
	}
	
	/**
	 * Create Perception fabula element
	 * 
	 * @param characterURI character of the perception
	 * @param contentTriple list of triples that are contained in this perception
	 * @param contentFabula list of fabula elements contained in this perception
	 * @return a new StoryPerception
	 */
	public static StoryPerception createPerception(String characterURI, List contentTriple, List contentFabula) {
		StoryPerception sp = new StoryPerception();
		String newSP = UniqueId.generateUniqueIndividual(
				"Perception", PrologKB.fromNrSign(characterURI));
		sp.setIndividual(newSP);
		sp.setCharacter(characterURI);
		sp.setContentTriple(contentTriple);
		sp.setContentFabula(contentFabula);
		sp.setType(Fabula.Perception);
		sp.setTime(StoryTime.getTime());
		return sp;
	}	
	
	/**
	 * Create setting fabula element
	 * 
	 * @param characterURI character of the setting
	 * @param contentTriple list of triples that are contained in this setting
	 * @param contentFabula list of fabula elements contained in this setting
	 * @return a new StorySettingElement
	 */
	public static StorySettingElement createSettingElement(String characterURI, List contentTriple, List contentFabula) {
		StorySettingElement sse = new StorySettingElement();
		String newSE = UniqueId.generateUniqueIndividual(
				"Setting", PrologKB.fromNrSign(characterURI));
		sse.setIndividual(newSE);
		sse.setCharacter(characterURI);
		sse.setContentTriple(contentTriple);
		sse.setContentFabula(contentFabula);
		sse.setType(Fabula.SettingElement);
		sse.setTime(StoryTime.getTime());
		return sse;
	}
	
	/**
	 * Create BeliefElement fabula element
	 * @param characterURI character of the belief
	 * @param contentTriple list of triples that are contained in this belief
	 * @param contentFabula list of fabula elements contained in this belief
	 * @return a new StoryBelief
	 */
	public static StoryBelief createStoryBelief(String characterURI, List contentTriple, List contentFabula) {
		StoryBelief b = new StoryBelief();
		b.setCharacter(characterURI);
		b.setIndividual(UniqueId.generateUniqueIndividual("Belief",
				PrologKB.fromNrSign(characterURI)));
		b.setType(Fabula.BeliefElement);
		b.setTime(StoryTime.getTime());
		b.setContentTriple(contentTriple);
		b.setContentFabula(contentFabula);
		return b;
	}	
	
	/**
	 * Create Outcome fabula element
	 * 
	 * @param outcome what kind of outcome to create (success, failure, etc)
	 * @param characterURI character of the Outcome
	 * @return a new StoryOutcome
	 */
	public static StoryOutcome createStoryOutcome(Outcome outcome, String characterURI) {
		StoryOutcome so = new StoryOutcome();
		so.setCharacter(characterURI);
		so.setIndividual(UniqueId.generateUniqueIndividual("Outcome",
				PrologKB.fromNrSign(characterURI)));

		if (outcome == Outcome.positive) {
			so.setType(Fabula.SuccessOutcome);	
		} else if (outcome == Outcome.negative) {
			so.setType(Fabula.FailureOutcome);
		} else if (outcome == Outcome.neutral) {
			so.setType(Fabula.NeutralOutcome);
		}
		so.setTime(StoryTime.getTime());
		return so;
	}
}
