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

import vs.communication.FabulaCausality;
import vs.communication.FabulaElement;
import vs.fabula.FabulaCollector;

/**
 * Fabula collector for Character Agents' episodic memory. Decorates it to make sure that whenever fabula enters the
 * collector, the agent immediately stores it in its own episodic memory KB as well. This is necessary for querying
 * episodic context in the reasoning processes of the character, as well as for further explanation purposes (e.g., 
 * retrieving the beliefs that enabled a goal)
 * 
 * @author swartjes
 */
public class EpisodicMemoryCollector extends FabulaCollector {

	protected EpisodicMemory _epiMemory;
	
	public EpisodicMemoryCollector(EpisodicMemory em) {
		_epiMemory = em;
	}
	
	@Override
	public void addFabulaCausality(FabulaCausality fc) {
		super.addFabulaCausality(fc);
		_epiMemory.addFabulaCausality(fc);
	}
	
	@Override
	public void addFabulaElement(FabulaElement fe) {
		super.addFabulaElement(fe);		
		_epiMemory.addFabulaElement(fe);
	}	
}
