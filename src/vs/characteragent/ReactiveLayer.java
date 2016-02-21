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

import java.util.Vector;
import java.util.logging.Logger;

import vs.communication.StoryAction;
import vs.debug.LogFactory;
import vs.fabula.FabulaFactory;
import vs.knowledge.PrologKB;
import vs.utils.Chooser;

public class ReactiveLayer extends BehaviourLayer {
	
	public Logger logger;
	
	public ReactiveLayer(ICharacterAgent owner) {
		super(owner);
		logger = LogFactory.getLogger(this);
	}

	@Override
	public void appraise() {
		// TODO Auto-generated method stub

	}

	@Override
	public void cope() {
		// TODO Auto-generated method stub

	}

	@Override
	public StoryAction selectAction() {
		// TODO Auto-generated method stub
		Vector<String> reactiveActions = PrologKB.getInstance().selectReactiveActions(getAgent().getCharacterURI());
		if (reactiveActions == null || reactiveActions.isEmpty()) {
			logger.info("Select action: no reactive actions.");
			return null;
		}
		logger.info("Select action: choosing one of the reactive actions.");	
		
		StoryAction selectedAction = FabulaFactory.createAction(
					Chooser.randomChoice(reactiveActions), getAgent().getCharacterURI());
		
		_fabulaCollector.addFabulaElement(selectedAction);
		
		return selectedAction;
	}
	

}
