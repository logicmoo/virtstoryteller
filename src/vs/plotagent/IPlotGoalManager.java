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

import vs.IAgentModule;

/**
 * @author tommasse
 *
 */
public interface IPlotGoalManager extends IAgentModule {

	public interface IPlotGoalResult {
		public void handleFailure ();
		public void handleSuccess ();
	}
	
	public class PlotGoal {
		private String conditions;
		private IPlotGoalResult result;
		
		public PlotGoal(PlotGoal goal) {
			this.conditions = goal.conditions;
			this.result = goal.result;
		}
		
		public PlotGoal (String conditions, IPlotGoalResult result) {
			this.conditions = conditions;
			this.result = result;
		}
		
		public void failure () {
			if (result != null) result.handleFailure ();
		}
		
		public String getConditions() {
			return conditions;
		}
		
		public String toString () {
			return conditions;
		}
		
		public void success () {
			if (result != null) result.handleSuccess ();
		}
	}
	
	void addGoal (PlotGoal goal);
	
	/**
	 * Tries to perform new steps to achieve the plot goals
	 * @param fail if true, the failure condition of a plot goal should be executed if no plan can be made
	 */
	void performSteps (boolean fail);
	
}
