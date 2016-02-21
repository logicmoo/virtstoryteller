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

import java.util.Vector;
import java.util.logging.Logger;

import vs.debug.LogFactory;
import vs.plotagent.BasicPlotAgent;

public class ManageEpisodesBehaviour extends SimpleBehaviour {

	BasicPlotAgent plotAgent;
	Logger logger;
	
	public ManageEpisodesBehaviour(BasicPlotAgent arg0) {
		super(arg0.getAgent());
		logger = LogFactory.getLogger(this);
		plotAgent = arg0;
	}

	@Override
	public void action() {
		Logger logger = LogFactory.getLogger(this);
		
		plotAgent.writeConsole("Starting behaviour: episode management");
		
		if (!plotAgent.getThreadManager().startingThread()) {
			// Start new one
			Vector<String> possibleEpisodes = plotAgent.getThreadManager().getPossibleThreads();
			logger.info("Possible episodes at this point: " + possibleEpisodes);
			if (possibleEpisodes.iterator().hasNext()) {
				plotAgent.getThreadManager().addThread(possibleEpisodes.iterator().next());
			} else {
				logger.severe("No episodes possible at this point! Make sure there are episode definitions in the domain and at least one that is always possible for the given domain.");
			}
		}
	}
	
	@Override
	public boolean done() {
		// TODO Auto-generated method stub
		return true;
	}

}
