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
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.logging.Logger;

import vs.IExplainable;
import vs.characteragent.behaviour.InitiateRequestPlotPerformOperatorBehaviour;
import vs.communication.FabulaCausality;
import vs.communication.FabulaElement;
import vs.communication.FramingOperator;
import vs.communication.GoalSchema;
import vs.communication.StoryAction;
import vs.communication.StoryEvent;
import vs.communication.StoryOutcome;
import vs.debug.LogFactory;
import vs.fabula.FabulaFactory;
import vs.knowledge.PrologKB;
import vs.poplanner.PlanStep;
import vs.poplanner.PoPlanner;
import vs.rationalagent.StoryTime;
import vs.utils.Chooser;
import vs.utils.UniqueId;

public class ActorProcess extends CharacterProcess implements IExplainable {

	protected Logger logger;
	protected ICharacterAgent m_characterAgent;
	
	/**
	 * Represents the out-of-character process of the agent-as-director. Responsible for tasks such as 
	 * justifying goals, and executing events & framing operators.
	 * 
	 * @param ownerAgent the Character Agent that runs this process
	 */
	public ActorProcess(ICharacterAgent ownerAgent) {
		super(ownerAgent);
		logger = LogFactory.getLogger(this);
		m_characterAgent = ownerAgent;
		
		m_reactiveProcess = new ReactiveLayer(ownerAgent);
		m_deliberativeProcess = new ActorDeliberativeLayer(ownerAgent);
	}
	
	public ActorDeliberativeLayer getDeliberativeLayer() {
		return (ActorDeliberativeLayer)m_deliberativeProcess;
	}
		
	/**
	 * For now, the DirectorProcess does not produce causalities that needs to end up in the fabula directly. Maybe it stays that way, maybe later
	 * there are such causalities.
	 * 
	 * @see IExplainable
	 */
	public Set<FabulaCausality> explainCausalities() {
		// TODO
		return super.explainCausalities();
	}
	
	/**
	 * For now, the DirectorProcess does not produce elements that needs to end up in the fabula directly. Maybe it stays that way, maybe later
	 * there are such elements.
	 * 
	 * @see IExplainable
	 */
	public Set<FabulaElement> explainElements() {
		// TODO
		return super.explainElements();
	}
	
	
}
