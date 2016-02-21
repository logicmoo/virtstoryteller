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
package vs.knowledge.vocab;

import vs.Config;

public class Fabula {

	public static final String FABULA_NS = Config.namespaceMap
			.get(Config.FABULA_PREFIX);

	// CLASSES
	public static String FabulaElement = Fabula.FABULA_NS + "FabulaElement";
	public static String SettingElement = Fabula.FABULA_NS + "SettingElement";
	public static String Event = Fabula.FABULA_NS + "Event";
	public static String Action = Fabula.FABULA_NS + "Action";
	public static String InternalElement = Fabula.FABULA_NS + "InternalElement";
	public static String BeliefElement = Fabula.FABULA_NS + "BeliefElement";
	public static String Goal = Fabula.FABULA_NS + "Goal";
	public static String Perception = Fabula.FABULA_NS + "Perception";
	public static String AttainGoal = Fabula.FABULA_NS + "AttainGoal";
	public static String LeaveGoal = Fabula.FABULA_NS + "LeaveGoal";
	public static String MaintainGoal = Fabula.FABULA_NS + "MaintainGoal";
	public static String AvoidGoal = Fabula.FABULA_NS + "AvoidGoal";
	public static String Outcome = Fabula.FABULA_NS + "Outcome";
	public static String SuccessOutcome = Fabula.FABULA_NS + "Success";
	public static String FailureOutcome = Fabula.FABULA_NS + "Failure";
	public static String NeutralOutcome = Fabula.FABULA_NS + "Neutral";
	
	public static String TruthGraph = Fabula.FABULA_NS + "TruthGraph";
	public static String FalsehoodGraph = Fabula.FABULA_NS + "FalsehoodGraph";

	// RELATIONSHIPS
	public static String agens = Fabula.FABULA_NS + "agens";
	public static String patiens = Fabula.FABULA_NS + "patiens";
	public static String target = Fabula.FABULA_NS + "target";
	public static String instrument = Fabula.FABULA_NS + "instrument";
	public static String time = Fabula.FABULA_NS + "time";
	public static String starttime = Fabula.FABULA_NS + "starttime";
	public static String endtime = Fabula.FABULA_NS + "endtime";

	public static String character = Fabula.FABULA_NS + "character";

	public static String causes = Fabula.FABULA_NS + "causes";
	public static String phi_causes = Fabula.FABULA_NS + "phi_causes";
	public static String psi_causes = Fabula.FABULA_NS + "psi_causes";
	public static String motivates = Fabula.FABULA_NS + "motivates";
	public static String enables = Fabula.FABULA_NS + "enables";
	
	public static String resolves = Fabula.FABULA_NS + "resolves";

	public static String hasContent = Fabula.FABULA_NS + "hasContent";
	public static String hasTruth = Fabula.FABULA_NS + "hasTruth";
}
