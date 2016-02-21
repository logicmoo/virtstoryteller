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

import java.util.Set;

import vs.IAgentModule;
import vs.IExplainable;
import vs.communication.StoryBelief;
import vs.communication.StoryPerception;
import vs.communication.StorySettingElement;

/**
 * Interface for translating ("objective") perceptions into (subjective) beliefs.
 * This could be influenced by character dispositions, or the current state of mind of the agent.
 * 
 * The trivial interpretation would be to directly make perceptions into beliefs.
 * 
 * @author swartjes
 *
 */
public interface IInterpretationModule extends IAgentModule, IExplainable {
	
	/**
	 * Just adopts a setting element as a belief.
	 * @param sse the new setting element
	 * @return a set of beliefs as a result of the setting element
	 */
	public Set<StoryBelief> adopt(StorySettingElement sse);
	
	/**
	 * Take a perception, and interpret it in order to form beliefs
	 * 
	 * @param sp the perception
	 * @return a set of formed beliefs
	 */
	public Set<StoryBelief> interpret(StoryPerception sp);
	
	/**
	 * Takes a belief, and sees whether that results in further beliefs
	 * (e.g. believing someone lies on the floor -> believing someone is dead)
	 * 
	 * @param sb a belief to re-interpret
	 * @return a set of new story beliefs, or the empty set (if no new beliefs are formed)
	 */
	public Set<StoryBelief> reinterpret(StoryBelief sb);
}
