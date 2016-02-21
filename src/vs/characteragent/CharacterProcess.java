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
package vs.characteragent;

import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

import vs.IExplainable;
import vs.communication.FabulaCausality;
import vs.communication.FabulaElement;
import vs.communication.StoryAction;
import vs.communication.StoryBelief;
import vs.debug.LogFactory;

/**
 * CharacterProcess is meant to capture the step cycle that an agent goes through,
 * in which it appraises and copes with changes in its environment. A character process
 * is also responsible for selecting a way to act, based on the process. 
 * 
 * @author swartjes
 */
public class CharacterProcess extends BehaviourLayer implements IExplainable {
	
	protected Logger logger;
	protected ReactiveLayer m_reactiveProcess;
	protected DeliberativeLayer m_deliberativeProcess;
	
	/**
	 * Constructor
	 * 
	 * @param agent the owner of this character process
	 */
	public CharacterProcess(ICharacterAgent agent) {
		super(agent);
		logger = LogFactory.getLogger(this);
		
	}
	
	/**
	 * Retrieves the reactive layer (i.e., reactive rule based layer) of this character process
	 * 
	 * @return a reactive layer
	 */
	public ReactiveLayer getReactiveLayer() {
		return m_reactiveProcess;
	}
	
	/**
	 * Retrieves the deliberative layer (i.e., goals and plans based layer) of this character process
	 * @return the deliberative layer
	 */
	public DeliberativeLayer getDeliberativeLayer() {
		return m_deliberativeProcess;
	}	
	
	/**
	 * Returns the character agent that this character process belongs to
	 */
	public ICharacterAgent getAgent() {
		return m_characterAgent;
	}
	
	
	/* See IExplainable */
	public Set<FabulaCausality> explainCausalities() {
		Set<FabulaCausality> causalities = super.explainCausalities();
		causalities.addAll(getDeliberativeLayer().explainCausalities());
		causalities.addAll(getReactiveLayer().explainCausalities());

		return causalities;
	}
		
	/* See IExplainable */
	public Set<FabulaElement> explainElements() {
		Set<FabulaElement> elements = super.explainElements();
		elements.addAll(getDeliberativeLayer().explainElements());
		elements.addAll(getReactiveLayer().explainElements());

		return elements;
	}
	
	/**
	 * Performs cognitive appraisal
	 */
	public void appraise() {
		m_reactiveProcess.appraise();
		m_deliberativeProcess.appraise();
	}
	
	/**
	 * Performs coping
	 */
	public void cope() {
		m_reactiveProcess.cope();
		m_deliberativeProcess.cope();
	}	
	
	/**
	 * Selects and returns an action that the character would want to perform.
	 */
	public StoryAction selectAction() {
		// First try to get a reactive action; then a deliberative action.
		StoryAction selectedAction = getReactiveLayer().selectAction();
		if (selectedAction == null) {
			selectedAction = getDeliberativeLayer().selectAction();
		}

		return selectedAction;
	}
		
	
}
