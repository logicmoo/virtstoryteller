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
package vs.characteragent;

import jade.util.leap.Iterator;

import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

import vs.communication.FabulaCausality;
import vs.communication.FabulaElement;
import vs.communication.RDFtriple;
import vs.communication.StoryBelief;
import vs.communication.StoryPerception;
import vs.communication.StorySettingElement;
import vs.debug.LogFactory;
import vs.fabula.FabulaFactory;
import vs.knowledge.PrologKB;
import vs.knowledge.vocab.Fabula;

public class BasicInterpretationModule implements IInterpretationModule {

	protected ICharacterAgent m_ownerAgent;
	protected Logger logger;
	
	protected EpisodicMemoryCollector _fabulaCollector;
	
	public BasicInterpretationModule(ICharacterAgent owner) {
		m_ownerAgent = owner;
		logger = LogFactory.getLogger(this);
	
		_fabulaCollector = new EpisodicMemoryCollector(m_ownerAgent.getEpisodicMemory());
	}
	
	/**
	 * Just adopts new setting elements as beliefs
	 * 
	 * @param sse the new setting
	 * @return a set of adopted beliefs.
	 */
	public Set<StoryBelief> adopt(StorySettingElement sse) {
		logger.info("Faking belief from: " + sse.getType() + ": \n" + sse.getContentTriple());
		
		_fabulaCollector.addFabulaElement(sse);  // Add perception to fabula
		
		HashSet<StoryBelief> beliefs = new HashSet<StoryBelief>();
		
		StoryBelief b = FabulaFactory.createStoryBelief(m_ownerAgent.getCharacterURI(), sse.getContentTriple(), sse.getContentFabula());

		beliefs.add(b);
		
		// Adopt it straight into the belief base (without forming perceptions)
		processBeliefs(beliefs, sse);
		
		return beliefs;
	}
	
	public ICharacterAgent getAgent() {	
		return m_ownerAgent;
	}
	
	/**
	 * Interprets perceptions. 
	 * Current implementation: just translate directly
	 * Planned implementation: the domain knowledge contains belief Schemas (next to action schemas, goal schemas etc)
	 * 		A belief schema is an operator that specifies under which circumstances certain beliefs are adopted. E.g. a belief
	 * 		schema could be that if you see someone lying on the floor, and their eyes are closed, and you don't know why, 
	 * 		you might believe they are sleeping (or dead).
	 * 		The implementation of interpret(StoryPerception) is then to see which belief schemas are applicable (their preconditions
	 * 		hold), and to assert their effects as beliefs. 
	 * 		See also FAtiMA's implementation of the use of inference operators. Maybe only interpret "newly perceived" information, 
	 * 		i.e. take the subset of belief schemas that match _before_ the perception was asserted, and belief schemas that match
	 * 		_after_ the perception was asserted. 
	 */
	public Set<StoryBelief> interpret(StoryPerception sp) {
		logger.info("Creating new belief from: " + sp.getType() + ": \n" + sp.getContentTriple());

		_fabulaCollector.addFabulaElement(sp);  // Add perception to fabula
		
		HashSet<StoryBelief> beliefs = new HashSet<StoryBelief>();
	
		StoryBelief b = FabulaFactory.createStoryBelief(m_ownerAgent.getCharacterURI(), sp.getContentTriple(), sp.getContentFabula());
		
		//StoryBelief b = FabulaFactory.getInstance().createBelief(sp);
		beliefs.add(b);
		
		processBeliefs(beliefs, sp);
		

		// Re-interpret initial beliefs
		Set<StoryBelief> reinterpretedBeliefs = reinterpret(b);
		beliefs.addAll(reinterpretedBeliefs);
		
		return beliefs;
	}


	/**
	 * Re-interprets beliefs to generate new beliefs
	 * Current implementation: do not do any re-interpretation.
	 * Planned implementation: see planned implementation of interpret(StoryPerception)
	 */
	public Set<StoryBelief> reinterpret(StoryBelief sb) {		
		HashSet<StoryBelief> newBeliefs = new HashSet<StoryBelief>(); 
		
		processBeliefs(newBeliefs, sb);
		
		// Re-reinterpret by recursion
		for (StoryBelief sb_i: newBeliefs) {
			newBeliefs.addAll(reinterpret(sb_i));
		}
		
		return newBeliefs;
	}

	
	public Set<FabulaCausality> explainCausalities() {
		return _fabulaCollector.explainCausalities();
	}
	
	public Set<FabulaElement> explainElements() {
		return _fabulaCollector.explainElements();
	}
	
	
	private void processBeliefs(Set<StoryBelief> sbSet, FabulaElement cause) {
		for (StoryBelief sb: sbSet) {
			
			assertBelief(sb);
			
			if (cause != null) {
		
				FabulaCausality fc = new FabulaCausality();
				fc.setSubjectIndividual(cause.getIndividual());
				fc.setObjectIndividual(sb.getIndividual());
				if (cause instanceof StorySettingElement) {
					fc.setCausalProperty(Fabula.enables); // settings enable beliefs
				} else {
					fc.setCausalProperty(Fabula.psi_causes); // perceptions psychologically cause beliefs
				}

				_fabulaCollector.addFabulaCausality(fc);
			}

			_fabulaCollector.addFabulaElement(sb);
		}
	}
	
	private void assertBelief(StoryBelief sb) {
		Iterator pI = sb.getAllContentTriple();
		RDFtriple t = new RDFtriple();
		while (pI.hasNext()) {
			t = (RDFtriple) pI.next();
			PrologKB.getInstance().tellRDFtriple(t);
		}
	}	
	
}
