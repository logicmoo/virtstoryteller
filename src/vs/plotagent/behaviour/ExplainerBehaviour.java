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

import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

import vs.communication.FabulaCausality;
import vs.communication.FabulaElement;
import vs.debug.LogFactory;
import vs.plotagent.BasicPlotAgent;
import vs.rationalagent.RationalAgent;

/**
 * Collects fabula elements and causalities from the character agent, and sends them to the Plot Agent
 * Also keeps track of what the Plot Agent already received (although principally and by implementation it is no problem
 * to send a fabula element twice, just that the PA already knew).
 * 
 * @author swartjes
 */
public class ExplainerBehaviour extends SimpleBehaviour {

	BasicPlotAgent plotAgent;
	Logger logger;
	
	Set<FabulaElement> explainedFabulaElements;
	Set<FabulaCausality> explainedFabulaCausalities;
	
	public ExplainerBehaviour(BasicPlotAgent arg0) {
		super(arg0.getAgent());
		logger = LogFactory.getLogger(this);
		plotAgent = arg0;
		
		explainedFabulaElements = new HashSet<FabulaElement>();
		explainedFabulaCausalities = new HashSet<FabulaCausality>();
	}

	@Override
	public void action() {
		Logger logger = LogFactory.getLogger(this);
		((RationalAgent)myAgent ).writeConsole("Starting behaviour: explainer behaviour");
		
		// Retrieve suggestions and determine which ones to send
		Set<FabulaElement> feExplanations = plotAgent.explainElements();
		feExplanations.removeAll(explainedFabulaElements);
		Set<FabulaCausality> feCausalities = plotAgent.explainCausalities();
		feCausalities.removeAll(explainedFabulaCausalities);
		
		// Send the remaining fabula
		for (FabulaElement fe: feExplanations) {
			plotAgent.getFabulaBuilder().addFabulaElement(fe);
			explainedFabulaElements.add(fe);
		}
		
		for (FabulaCausality fc: feCausalities) {
			plotAgent.getFabulaBuilder().addFabulaCausality(fc);
			explainedFabulaCausalities.add(fc);
		}

	}
	
	
	@Override
	public boolean done() {
		// This behaviour is always successful.
		return true;
	}

}
