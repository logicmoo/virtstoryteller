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
package vs.debug;

import java.util.Vector;

import vs.poplanner.PlanLink;
import vs.poplanner.PlanOrdering;
import vs.poplanner.PlanStep;
import vs.poplanner.PoPlanner;

public class DummyPlanner extends PoPlanner {

	protected Vector<PlanStep> _steps;
	protected Vector<PlanLink> _links;
	protected Vector<PlanOrdering> _orderings;
	
	public DummyPlanner() {
		super(null);
		
		_steps = new Vector<PlanStep>();
		_links = new Vector<PlanLink>();
		_orderings = new Vector<PlanOrdering>();
		
		PlanStep s1 = new PlanStep("s1", "", "Walk", "action");		
		PlanStep s2 = new PlanStep("s2", "", "StartsToRain", "event");
		PlanStep s3 = new PlanStep("s3", "", "GetWet", "expectation");
		PlanStep s4 = new PlanStep("s4", "", "HaveUmbrella", "framing");		
		PlanOrdering o1 = new PlanOrdering("s", "s1");
		PlanOrdering o2 = new PlanOrdering("s1", "s2");
		PlanOrdering o3 = new PlanOrdering("s2", "s3");
		PlanOrdering o4 = new PlanOrdering("s3", "s4");
		PlanLink l1 = new PlanLink("s", "s1", "test");
		_steps.add(s1);
		_steps.add(s2);
		_steps.add(s3);
		_steps.add(s4);
		_orderings.add(o1);
		_orderings.add(o2);
		_orderings.add(o3);
		_orderings.add(o4);
		_links.add(l1);
	}
	
	
	@Override
	public Vector<String> getExecutableEvents() {		return null;	}
	
	@Override
	public Vector<String> getExecutableFramingOperators() {		return null;	}	
	
	@Override
	public Vector<String> getExecutableOperators() {		return null;	}

	
	/**
	 * Retrieves all dummy causal links from the plan
	 */
	@Override
	public Vector<PlanLink> getLinks() {
		// TODO
		return _links;
	}
	/**
	 * Retrieves all dummy orderings from the plan
	 */
	@Override
	public Vector<PlanOrdering> getOrderings() {
		// TODO
		return _orderings;
	}
	@Override
	public String getPlan() {		return null;	}
	/**
	 * Retrieves dummy steps from the plan
	 */
	@Override
	public Vector<PlanStep> getSteps() {
		// TODO
		return _steps;
	}
	@Override
	public boolean plan() {		return false;	}	
	@Override
	public boolean planFinished() {		return false;	}
	@Override
	public void setGoals(String goals) {}
	@Override
	public void setPlan(String plan) {}
	
}
