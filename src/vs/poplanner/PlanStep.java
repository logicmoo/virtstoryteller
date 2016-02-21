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
package vs.poplanner;


/**
 * Represents a step in a plan. Immutable, only for getting information.
 * 
 * @author swartjes
 *
 */
public class PlanStep {
	
	private String name;
	private String operator;
	private String type;
	private String clss;
	
	/**
	 * Constructor 
	 *
	 * @param stepName	name of the step
	 * @param stepOperator	prolog description of the operator of the step
	 * @param operatorType	OWL class of the step
	 * @param operatorClass	kind of step (e.g., action, event, framing)
	 */
	public PlanStep(String stepName, String stepOperator, String operatorType, String operatorClass) {
		name = stepName;
		operator = stepOperator;
		type = operatorType;
		clss = operatorClass;
	}

	public String getName() {
		return name;
	}

	public String getOperator() {
		return operator;
	}
	
	public String getOperatorClass() {
		return clss;
	}
	
	public String getOperatorType() {
		return type;
	}	

}
