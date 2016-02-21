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

import jade.core.AID;
import vs.communication.FabulaCausality;
import vs.communication.FabulaCausalityDeclaration;
import vs.communication.FabulaElement;
import vs.communication.FabulaElementDeclaration;
import vs.fabula.BasicFabulaBuilder;
import vs.fabula.NamedGraphFabulaKnowledgeBase;
import vs.fabula.PrologFabulaKnowledgeBase;
import vs.rationalagent.behaviour.SendInformBehaviour;

/**
 * 	Logs fabula, but on top of this, also broadcasts fabula to character agents.
 * 
 * @author swartjes
 *
 */
public class BroadcastingFabulaBuilder extends BasicFabulaBuilder {

	public BroadcastingFabulaBuilder(IPlotAgent owner) {
		super(owner);
		
		registerFabulaKnowledgeBase(new NamedGraphFabulaKnowledgeBase());
		registerFabulaKnowledgeBase(new PrologFabulaKnowledgeBase());
	}
	
	@Override
	public void addFabulaCausality(FabulaCausality fc) {
		super.addFabulaCausality(fc);
		
		// send to character agents
		logger.info("Sending fabula causality to all characters");
		FabulaCausalityDeclaration fcd = new FabulaCausalityDeclaration();
		fcd.setFabulaCausality(fc);

		// Send to all characters, also those that do not play a role, since they are "witnessing" the fabula evolve.
		AID[] characters = ((IPlotAgent) getAgent()).getCharacterManager().getCharacters().toArray(new AID[0]);
		getAgent().getAgent().addBehaviour(new SendInformBehaviour(
													getAgent().getAgent(), 
													characters,
													fcd));		
	}
	
	/**
	 * Logs fabula, but on top of this, also broadcasts fabula to character agents.
	 */
	@Override
	public void addFabulaElement(FabulaElement fe) {
		super.addFabulaElement(fe);

		// send to character agents.
		FabulaElementDeclaration fed = new FabulaElementDeclaration();
		fed.setFabulaElement(fe);

		// Send to all characters, also those that do not play a role, since they are "witnessing" the fabula evolve.
		AID[] characters = ((IPlotAgent) getAgent()).getCharacterManager().getCharacters().toArray(new AID[0]);
		
		logger.info("Sending fabula element to all characters: " + fe.getType() + "\n" + characters);
		getAgent().getAgent().addBehaviour(new SendInformBehaviour(
													getAgent().getAgent(), 
													characters,
													fed));
		
	}
	

}
