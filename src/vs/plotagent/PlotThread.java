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

import java.util.Iterator;
import java.util.Vector;
import java.util.HashMap;
import java.util.logging.Logger;

import vs.communication.GoalSchema;
import vs.debug.LogFactory;
import vs.knowledge.PrologKB;
import vs.plotagent.behaviour.InitiateRequestUseSuggestionBehaviour;
import vs.utils.UniqueId;

public class PlotThread {

	private String prologName;
	
	private int state;
	
	private Vector<String> characters;
	private Vector<Goal> goals;
	private HashMap<String, Vector<Goal>> characterGoals;
	private HashMap<String, Vector<Goal>> resolveGoals;
	private Vector<String> settings;
	private String preconditions;
	
	private IThreadManager manager;

	private Logger logger;
	
	public static final int NONE = -1;
	public static final int STARTING = 0;
	public static final int STARTED = 1;
	public static final int FINISHED = 2;
	
	public PlotThread(IThreadManager manager, String prologName) {
		
		this.manager = manager;
		
		this.logger = LogFactory.getLogger(manager);

		this.prologName = prologName;
		preconditions = PrologKB.getInstance().getSchemaPreconditions(prologName);
		state = NONE;
		characters = PrologKB.getInstance().necessaryCharacters(prologName);
		goals = new Vector<Goal> ();
		characterGoals = new HashMap<String, Vector<Goal>> ();
		resolveGoals =  new HashMap<String, Vector<Goal>> ();
		for (int i = 0; i < characters.size (); ++i)
			characters.set (i, PrologKB.getInstance ().removeQuotes (characters.get (i)));
		for(String character : characters) {
			logger.info ("Querying goals from character: " + character + " in thread: " + prologName);			
			Vector<String> charGoalsStr = PrologKB.getInstance().threadGoals(prologName, character);
			Vector<Goal> charGoals = new Vector<Goal> ();
			for(String goal : charGoalsStr)
				charGoals.add(new Goal (goal, character));
			characterGoals.put (character, charGoals);
			goals.addAll (charGoals);
			
			logger.info ("Querying resolve goals from character: " + character + " in thread: " + prologName);
			Vector<String> resGoalsStr = PrologKB.getInstance().threadResolveGoals(prologName, character);
			Vector<Goal> resGoals = new Vector<Goal> ();
			for(String goal : resGoalsStr)
				resGoals.add(new Goal (goal, character));
			resolveGoals.put (character, resGoals);
			goals.addAll (resGoals);
		}
		settings = PrologKB.getInstance().threadSettings(prologName);
	}
	
	public Vector<String> getCharacters () {
		return characters;
	}
	
	public Vector<Goal> getGoals () {
			return goals;
	}
	
	public Vector<Goal> getGoals (String character) {
		return characterGoals.get (character);
	}
	
	public Vector<Goal> getResolveGoals (String character) {
		return resolveGoals.get (character);
	}
	
	public Vector<String> getSettings () {
		return settings;
	}
	
	public int getState () {
		return state;
	}
	
	public String getPrologName () {
		return prologName;
	}

	public String getPreconditions () {
		return preconditions;
	}
	
	public void finishGoal (Goal goal) {
		logger.info ("Character " + goal.getCharacter () + " has finished goal " + goal.getPrologDescription ());

		if (getGoals (goal.getCharacter ()).contains (goal)) {
			for (Goal res : getResolveGoals (goal.getCharacter ()))
				manager.assignResolutionGoal (res, this);
		} else {
			logger.info ("Character " + goal.getCharacter () + " completed his resolve goal");
			manager.finishThread (this);
		}
	}
	
	public String toString () {
		return prologName;
	}
	
	public void setState (int state) {
		this.state = state;
	}

}
