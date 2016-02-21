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
 * Represents an ordering in a plan. Immutable, only for getting information.
 * 
 * @author swartjes
 *
 */
public class PlanOrdering {
	
	private String src;
	private String target;
	
	/**
	 * Constructor 
	 * @param srcStepName name of the step in the plan that the link comes from
	 * @param targetStepName name of the step in the plan that the link goes to
	 */
	public PlanOrdering(String srcStepName, String targetStepName) {
		src = srcStepName;
		target = targetStepName;
	}

	public String getSrc() {
		return src;
	}

	public String getTarget() {
		return target;
	}

}
