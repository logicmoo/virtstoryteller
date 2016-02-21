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
import jade.util.leap.ArrayList;
import jade.util.leap.List;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.logging.Logger;

import vs.communication.FabulaCausality;
import vs.communication.FabulaElement;
import vs.communication.Finished;
import vs.communication.FramingOperator;
import vs.communication.IncomingPerception;
import vs.communication.IncomingSetting;
import vs.communication.OperatorResult;
import vs.communication.RDFtriple;
import vs.communication.StoryAction;
import vs.communication.StoryEvent;
import vs.communication.StoryPerception;
import vs.communication.StorySettingElement;
import vs.communication.WorldChange;
import vs.debug.LogFactory;
import vs.fabula.FabulaCollector;
import vs.fabula.FabulaFactory;
import vs.knowledge.PrologKB;
import vs.knowledge.vocab.Fabula;
import vs.rationalagent.behaviour.SendInformBehaviour;

public class BasicPerceptionManager implements IPerceptionManager {

	/** The Plot Agent that owns this module * */
	private final IPlotAgent m_ownerAgent;
	private final Vector<OperatorResult> m_operatorResultVector;
	private final Vector<StorySettingElement> m_settingHistory;
	private final Vector<StoryPerception> m_perceptionHistory;
	private final Map<StoryPerception, FabulaElement> m_perceptionCauses;
	private final Map<AID, Vector<Object>> m_characterAwarenessMap;
	private final FabulaCollector m_fabulaCollector;

	protected Logger logger;

	public BasicPerceptionManager(IPlotAgent owner) {
		logger = LogFactory.getLogger(this);
		m_ownerAgent = owner;

		m_operatorResultVector = new Vector<OperatorResult>();
		m_settingHistory = new Vector<StorySettingElement>();
		m_perceptionHistory = new Vector<StoryPerception>();
		m_perceptionCauses = new HashMap<StoryPerception, FabulaElement>();
		m_characterAwarenessMap = new HashMap<AID, Vector<Object>>();
		m_fabulaCollector = new FabulaCollector();
		
	}

	public Set<FabulaCausality> explainCausalities() {
		HashSet<FabulaCausality> newCausalities = new HashSet<FabulaCausality>();
		for (FabulaCausality fc: m_fabulaCollector.explainCausalities()) {
			newCausalities.add(fc);
		}
		m_fabulaCollector.resetFabulaCausalities();
		return newCausalities;		
	}

	public Set<FabulaElement> explainElements() {
		HashSet<FabulaElement> newElements = new HashSet<FabulaElement>();
		for (FabulaElement fe: m_fabulaCollector.explainElements()) {
			newElements.add(fe);
		}
		m_fabulaCollector.resetFabulaElements();
		return newElements;		
	}

	public IPlotAgent getAgent() {
		return m_ownerAgent;
	}
	
	/**
	 * Sends new perceptions and setting elements to the characters
	 */
	public void informCharacters() {
		for (AID character: m_ownerAgent.getCharacterManager().getCastedCharacters()) {
			Vector<Object> knows = m_characterAwarenessMap.get(character);
			if (knows == null) {
				knows = new Vector<Object>();
			}
			
			// Setting elements
			for (StorySettingElement sse: m_settingHistory) {
				if (! knows.contains(sse)) {
					// New setting element for character
					knows.add(sse);
								
					// Send!
					IncomingSetting is = new IncomingSetting();
					is.setSetting(sse);
					
					AID[] receiver = new AID[1];
					receiver[0] = character;
					for (AID c: receiver) {
						logger.info("Sending setting element: " + sse.getIndividual() + "\nto character: " + c);
					}
					m_ownerAgent.getAgent().addBehaviour(new SendInformBehaviour(m_ownerAgent.getAgent(),
							receiver, is));
				}
				
			}
			
			// Perceptions
			for (StoryPerception p_prototypical: m_perceptionHistory) {
				if (! knows.contains(p_prototypical)) {
					knows.add(p_prototypical);
					
					StoryPerception p = FabulaFactory.createPerception(
							m_ownerAgent.getCharacterManager().getStoryWorldRepresentationForAgent(character), 
							p_prototypical.getContentTriple(), null);
					
					// Log
					m_fabulaCollector.addFabulaElement(p);
					
					FabulaElement p_cause = m_perceptionCauses.get(p_prototypical);
					
					if (p_cause != null) {
						FabulaCausality fc = new FabulaCausality();
						fc.setSubjectIndividual(m_perceptionCauses.get(p_prototypical).getIndividual()); 
						fc.setObjectIndividual(p.getIndividual());
						fc.setCausalProperty(Fabula.phi_causes);
						m_fabulaCollector.addFabulaCausality(fc);
					} else {
						logger.severe("Perception without causality: " + p_prototypical);
					}
					
					// Send!
					IncomingPerception ip = new IncomingPerception();
					ip.setPerception(p);
					
					AID[] receiver = new AID[1];
					receiver[0] = character;
					for (AID c: receiver) {
						logger.info("Sending perceptions to character: " + c);
					}
					m_ownerAgent.getAgent().addBehaviour(new SendInformBehaviour(m_ownerAgent.getAgent(),
							receiver, ip));
					
				}
			}
			
			// Operator results
			// TODO decide if we want this (peter sees that john successfully built a house) because its now an implicit effect of an action anyway
			// ...
			
			m_characterAwarenessMap.put(character, knows);
		}
	}

	public void registerOperatorResult(OperatorResult or) {
		m_operatorResultVector.add(or);
		if (	   or.getOperator() instanceof StoryAction 
				|| or.getOperator() instanceof StoryEvent) {
			// These are the operators with a duration. They are already in the fabula but now their
			// end time is known. So add again to ensure this gets logged.
			m_fabulaCollector.addFabulaElement(or.getOperator());
		}
		
		// Only proceed to create perceptions / settings if we are dealing with a finished operator
		if (! (or.getStatus() instanceof Finished)) {
			return;
		}
		
		// Make prototypical perceptions and setting elements
		Set<StoryPerception> perceptions = new HashSet<StoryPerception>();
		Set<StorySettingElement> settingElements = new HashSet<StorySettingElement>();
		Vector<RDFtriple> effects = PrologKB.getInstance().getOperatorEffects(or.getOperator().getPrologDescription());

		// Make prototypical perceptions
		if (	   or.getOperator() instanceof StoryAction 
				|| or.getOperator() instanceof StoryEvent) {
			perceptions = new HashSet<StoryPerception>();
			for (RDFtriple eff: effects) {
				StoryPerception p = new StoryPerception();
				p.addContentTriple(eff);
				p.setType(Fabula.Perception);
				perceptions.add(p);
				m_perceptionCauses.put(p, or.getOperator());
				
				logger.info("Created perception for effect: " + eff + "\nof operator: " + or.getOperator().getIndividual());
			}
		}
		
		// Make setting changes
		if (or.getOperator() instanceof FramingOperator) {
			settingElements = new HashSet<StorySettingElement>();
			for (RDFtriple eff: effects) {
				
				List contTriple = new ArrayList();
				contTriple.add(eff);
				settingElements.add(FabulaFactory.createSettingElement("plotagent", contTriple, null ));
				
				logger.info("Created setting element for effect: " + eff + "\nof framing operator: " + or.getOperator().getIndividual());
			}			
		}

		// Log setting elements to fabula (perceptions will follow per-character upon retrieval)
		for (StorySettingElement sse: settingElements) {
			logger.info("Registering fabula element for fabula: " + sse.getIndividual());
			m_fabulaCollector.addFabulaElement(sse);
		}
		
		// Remember, in order to be able to determine perceptions and settings for characters later on.
		m_settingHistory.addAll(settingElements);
		m_perceptionHistory.addAll(perceptions);
		
	}

}
