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
package vs.characteragent.behaviour;

import jade.core.AID;
import jade.core.behaviours.SimpleBehaviour;

import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

import vs.characteragent.BasicCharacterAgent;
import vs.communication.FabulaCausality;
import vs.communication.FabulaCausalityDeclaration;
import vs.communication.FabulaElement;
import vs.communication.FabulaElementDeclaration;
import vs.debug.LogFactory;
import vs.rationalagent.RationalAgent;
import vs.rationalagent.behaviour.SendInformBehaviour;

/**
 * Collects fabula elements and causalities from the character agent, and sends them to the Plot Agent
 * Also keeps track of what the Plot Agent already received (although principally and by implementation it is no problem
 * to send a fabula element twice, just that the PA already knew).
 * 
 * @author swartjes
 */
public class ExplainerBehaviour extends SimpleBehaviour {

	BasicCharacterAgent characterAgent;
	Logger logger;
	
	Set<FabulaElement> explainedFabulaElements;
	Set<FabulaCausality> explainedFabulaCausalities;
	
	public ExplainerBehaviour(BasicCharacterAgent arg0) {
		super(arg0.getAgent());
		logger = LogFactory.getLogger(this);
		characterAgent = arg0;
		
		explainedFabulaElements = new HashSet<FabulaElement>();
		explainedFabulaCausalities = new HashSet<FabulaCausality>();
	}

	@Override
	public void action() {
		Logger logger = LogFactory.getLogger(this);
		((RationalAgent)myAgent ).writeConsole("Starting behaviour: explainer behaviour");
		
		// Retrieve suggestions and determine which ones to send
		Set<FabulaElement> feExplanations = characterAgent.explainElements();
		feExplanations.removeAll(explainedFabulaElements);
		Set<FabulaCausality> feCausalities = characterAgent.explainCausalities();
		feCausalities.removeAll(explainedFabulaCausalities);
		
		// Send the remaining fabula to the Plot Agent
		/* The assumption made is that the Plot Agent will send back fabula elements and causalities as soon as it receives them, 
		 * so in effect the explanations will "bounce back" and stored in the character agent as episodic knowledge. 
		 * However, there are race conditions where appraisal can occur _before_ these elements are received. Results of such race conditions:
		 * 	- Goals that query very recent context (e.g. an action just chosen) might not be adopted yet
		 *  - If the agent tries to see which beliefs enabled his new goal, he might not be aware of these beliefs yet (episodically speaking)
		 *  - etc.
		 *  
		 *   To avoid these problems, the Character Agent will log the fabula elements immediately and will receive a copy of these from the
		 *   Plot Agent as well. But the Character Agent might in the future treat these different, e.g., annotate them with the fact that it
		 *   is out-of-character knowledge. 
		 *   
		 *   UPDATE: is now moved to EpisodicMemoryCollector
		 */ 
		for (FabulaElement fe: feExplanations) {
			sendFabulaElement(fe);
			//characterAgent.getEpisodicMemory().addFabulaElement(fe); // To avoid race conditions, we immediately store these elements for ourselves
			explainedFabulaElements.add(fe);
		}
		
		for (FabulaCausality fc: feCausalities) {
			sendFabulaCausality(fc);
			//characterAgent.getEpisodicMemory().addFabulaCausality(fc); // To avoid race conditions, we immediately store these elements for ourselves
			explainedFabulaCausalities.add(fc);
		}

	}
	
	@Override
	public boolean done() {
		// This behaviour is always successful.
		return true;
	}

	/**
	 * Sends a fabula causality to the Plot Agent
	 * @param fc the fabula causality to send
	 */
	public void sendFabulaCausality(FabulaCausality fc) {
		logger.fine("Sending Fabula Causality: " + fc);
		FabulaCausalityDeclaration fcd = new FabulaCausalityDeclaration();
		fcd.setFabulaCausality(fc);
		characterAgent.addBehaviour(new SendInformBehaviour(characterAgent, new AID[] { characterAgent.getPlotAgent() },
				fcd));
	}
	
	/**
	 * Sends a fabula element to the Plot Agent
	 * @param fe the fabula element to send
	 */
	public void sendFabulaElement(FabulaElement fe) {
		logger.fine("Sending Fabula Element: " + fe);
		FabulaElementDeclaration fed = new FabulaElementDeclaration();
		fed.setFabulaElement(fe);
		characterAgent.addBehaviour(new SendInformBehaviour(characterAgent, new AID[] { characterAgent.getPlotAgent() },
				fed));
	}

}
