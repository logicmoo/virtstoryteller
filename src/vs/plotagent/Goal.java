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

import vs.communication.GoalSchema;
import vs.knowledge.PrologKB;

public class Goal {
	private GoalSchema schema;
	
	public Goal (String prologDescription, String character) {
		schema = new GoalSchema ();
		schema.setPrologDescription (prologDescription);
		schema.setCharacter (character);
		schema.setAgens (nullIsEmpty (PrologKB.getInstance ().getSchemaAgens (prologDescription)));
		schema.setPatiens (nullIsEmpty (PrologKB.getInstance ().getSchemaPatiens (prologDescription)));
		schema.setOpponent (nullIsEmpty (PrologKB.getInstance ().getSchemaOpponent (prologDescription)));
		schema.setInstrument (nullIsEmpty (PrologKB.getInstance ().getSchemaInstrument(prologDescription)));
		schema.setTarget (nullIsEmpty (PrologKB.getInstance ().getSchemaTarget(prologDescription)));
		schema.setType (nullIsEmpty (PrologKB.getInstance ().getSchemaType (prologDescription)));
	}

	private String nullIsEmpty (String str) {
		if (str != null) return str;
		return "";		
	}
	
	public String getPrologDescription () {
		return schema.getPrologDescription ();
	}
	
	public String getCharacter () {
		return schema.getCharacter ();
	}
	
	/**
	 * Compares two goals; goals are considered the same if the characters they're assigned to
	 * are the same, while the type and arguments to the goal correspond. The arguments that are
	 * checked are:
	 * - agens
	 * - patiens
	 * - opponent
	 * - instrument
	 * - target
	 * If a new argument type gets introduced (that is somehow applicable to a goal), it should be added. 
	 */
	public boolean equals (Object obj) {
		if (obj instanceof Goal) {
			Goal g2 = (Goal) obj;
			return g2.getCharacter ().equals (getCharacter ()) 
				&& g2.schema.getType ().equals (schema.getType ())
				&& g2.schema.getAgens().equals (schema.getAgens())
				&& g2.schema.getPatiens().equals (schema.getPatiens())
				&& g2.schema.getOpponent().equals (schema.getOpponent())
				&& g2.schema.getInstrument().equals (schema.getInstrument())
				&& g2.schema.getTarget().equals (schema.getTarget());
			
			
		}
		return false;		
	}
	
	public String toString () {
		return "Character: " + getCharacter () + " Goal Schema: type (" + schema.getType () + ") agens (" + schema.getAgens () + ") patiens (" + schema.getPatiens () + ") opponent (" + schema.getOpponent () + ") instrument (" + schema.getInstrument () + ") target (" + schema.getTarget () + ")";
	}
}
