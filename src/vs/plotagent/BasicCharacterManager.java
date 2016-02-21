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

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.logging.Logger;

import vs.AgentLauncher;
import vs.communication.CharacterInfo;
import vs.communication.GoalSchema;
import vs.communication.RDFtriple;
import vs.debug.CastException;
import vs.debug.LogFactory;
import vs.knowledge.PrologKB;
import vs.plotagent.behaviour.InitiateRequestPlayCharacterBehaviour;
import vs.plotagent.behaviour.InitiateRequestUseSuggestionBehaviour;
import vs.utils.UniqueId;

/**
 * @author swartjes
 *
 */
public class BasicCharacterManager implements ICharacterManager {
	
	/**
	 * Information about the status and role of a character agent
	 * @author swartjes
	 *
	 */
	private class CastInformation {		
		AID agent;
		Status status;
		String characterURI;
		
		public CastInformation(AID agentAID) {
			agent = agentAID;
		}

		public AID getAID() {
			return agent;
		}
		
		public String getCharacterURI() {
			return characterURI;
		}
		
		public Status getStatus() {
			return status;
		}
		
		public void setCharacterURI(String uri) {
			characterURI = uri;
		}
		
		public void setStatus(Status s) {
			status = s;
		}
	}
	
	public enum Status {AVAILABLE, CASTED, UNAVAILABLE}
	// For safety, this prevents potential bugs in the code to continuously spawn new agents.
	// Assumption for now is that a story that needs more than 5 character agents is somehow "wrong".
	public static int MAX_CHAR_AGENTS = 5;

	protected Logger logger;
	
	protected IPlotAgent ownerAgent;
	
	protected Vector<RDFtriple> settings;
	
	private Map<AID, CastInformation> m_characterInfo;
	
	private Vector<String> m_castAdvertList;
	
	public BasicCharacterManager(IPlotAgent owner) {
		logger = LogFactory.getLogger(this);

		ownerAgent = owner;
		
		m_characterInfo = new HashMap<AID, CastInformation>();
		
		m_castAdvertList = new Vector<String>();
		
		settings = new Vector<RDFtriple>();
	}

	/* (non-Javadoc)
	 * @see vs.plotagent.ICharacterManager#addCharacter(jade.core.AID)
	 */
	public void addCharacterAgent(AID character) {
		
		if (! m_characterInfo.containsKey(character)) {
			CastInformation ci = new CastInformation(character);
			ci.setStatus(Status.AVAILABLE);
			m_characterInfo.put(character, ci);
			
			// If we are still in need of a role, try to cast the new character in the role.
			Iterator<String> it = m_castAdvertList.iterator();
			if (it.hasNext()) {
				tryCastCharacter(character, it.next());
			}
			
			
		}
		logger.log(jade.util.Logger.INFO, "New character agent subscribed: " + character.toString());
			
	}
	

	/**
	 * TODO: accept StorySettingElements rather than RDFtriples. (ivo)
	 */
	public void addSettingChange (Collection<RDFtriple> triples) {
		settings.addAll (triples);
	}

	/**
	 * TODO: accept StorySettingElements rather than RDFtriples. (ivo)
	 */
	public void addSettingChange (RDFtriple triple) {
		settings.add (triple);
	}

	public void addWantedCharacter(String characterURI) {

		// Put character URI in the list of "wanted" characters
		m_castAdvertList.add(characterURI);

		if (getAgentForStoryWorldRepresentation(characterURI) == null) {
			
			// We don't have this character casted yet
			Set<AID> availableCharacters = getAvailableCharacters();
			if (availableCharacters.iterator().hasNext()) {
				
				// Cast one of the available characters
				AID recruit = availableCharacters.iterator().next();
				logger.info("Casting character " + characterURI + " by agent " + recruit);
				tryCastCharacter(recruit, characterURI);
				
			} else {
							
				// Spawn new ones for all needed characters				
				for (String s: m_castAdvertList) {
					logger.info("Spawning new character agent due to shortage of available character agents");
					spawnCharacterAgent();
				}					
			}
		}
		// Else: there is already an agent playing this role; do nothing more.
	}
	
	/**
	 * This is executed under the assumption that the character AID is already committed to character URI.
	 * 
	 * @param agent the AID of the CharacterAgent we are casting
	 * @param characterURI the URI of the character in the story.
	 */
	public void castCharacter(AID agent, String characterURI) {
		CastInformation info = m_characterInfo.get(agent);
		
		try {
		
			if (info == null) {
				throw new CastException("Trying to cast a character for an agent which is not known to the Character Manager");
			}

			info.setStatus(Status.CASTED);
			info.setCharacterURI(characterURI);

			m_castAdvertList.remove(characterURI);
			
			logger.info("Casting agent " + agent.getLocalName() + "\nas character " + characterURI.toString());

/*			// Send the changed setting information to the character
			StorySettingElement sse = new StorySettingElement();
			sse.setIndividual(UniqueId.generateUniqueIndividual("Setting", "plotagent"));
			sse.setType(Fabula.SettingElement);
			sse.setCharacter("plotagent");
				
			// TODO: are all triples "positive"? or can you also have settings like "there was no beer."
			// TODO: this does not yet work.
			// TODO: do not combine settings into one big SettingElement; fabula does not like that. Keep them as they were.
				
				
			for (RDFtriple triple: settings) {
				logger.info("Adding triple to setting: " + triple);
				sse.addContentTriple(triple);
			}
				
			IncomingSetting is = new IncomingSetting();
			is.setSetting(sse);
				

			
			AID[] receiver = new AID[1];
			receiver[0] = agent;
			logger.info("Sending setting to character " + receiver[0]);
			ownerAgent.getAgent().addBehaviour(new SendInformBehaviour(ownerAgent.getAgent(), receiver, is));*/
	
			
			// Give the now casted agent its episodic goals.
			Vector<Goal> episodicGoals = getAgent().getThreadManager().getThreadGoals(characterURI);
			logger.info("Episodic goals for character " + characterURI + ":\n" + episodicGoals);			

			for (Goal goal: episodicGoals) {
	
				GoalSchema gs = new GoalSchema();
				gs.setCharacter(characterURI);
				String type = PrologKB.getInstance().getSchemaType(goal.getPrologDescription ());
				gs.setIndividual(UniqueId.generateUniqueIndividual("EpisodicGoal", PrologKB.fromNrSign(characterURI)));
				gs.setType(type);
				gs.setPrologDescription(goal.getPrologDescription ());
				ownerAgent.getAgent().addBehaviour(new InitiateRequestUseSuggestionBehaviour(ownerAgent.getAgent(),agent,gs));
			}
		} catch(CastException ce) {
			logger.severe("CastException: " + ce.getMessage());
		}
	}
	
	public IPlotAgent getAgent() {
		return ownerAgent;
	}
	
	/* (non-Javadoc)
	 * @see vs.plotagent.ICharacterManager#getAgentForStoryWorldRepresentation(String)
	 */
	public AID getAgentForStoryWorldRepresentation(String characterURI) {

		AID agent = null;
		
		for (Map.Entry<AID,CastInformation> e: m_characterInfo.entrySet()) {
			if (e.getValue().getCharacterURI() == null) {
				return null;
			}
			if (e.getValue().getCharacterURI().equals(characterURI)) {
				return e.getKey();
			}
		}

		return agent;
	}
	
	/* (non-Javadoc)
	 * @see vs.plotagent.ICharacterManager#getAvailableCharacters()
	 */
	public Set<AID> getAvailableCharacters() {
		Set<AID> availableCharacters = new HashSet<AID>();
		for (Map.Entry<AID,CastInformation> e: m_characterInfo.entrySet()) {
			if (e.getValue().getStatus() == Status.AVAILABLE) {
				availableCharacters.add(e.getKey());
			}
		}

		return availableCharacters;
	}

	/* (non-Javadoc)
	 * @see vs.plotagent.ICharacterManager#getCastedCharacters()
	 */
	public Set<AID> getCastedCharacters() {
		
		Set<AID> casted = new HashSet<AID>();
		for (Map.Entry<AID,CastInformation> e: m_characterInfo.entrySet()) {
			if (e.getValue().getStatus() == Status.CASTED) {
				casted.add(e.getKey());
			}
		}
		
		//return m_joinedCharacterAgents;
		return casted;
	}
	
	/* (non-Javadoc)
	 * @see vs.plotagent.ICharacterManager#getCharacters()
	 */
	public Set<AID> getCharacters() {

		//return m_subscribedCharacterAgents;
		return m_characterInfo.keySet();
	}

	/* (non-Javadoc)
	 * @see vs.plotagent.ICharacterManager#getStoryWorldRepresentationForAgent(jade.core.AID)
	 */
	public String getStoryWorldRepresentationForAgent(AID agent) {

		CastInformation info = m_characterInfo.get(agent);
		
		if (info == null) {
			return null;
		} else {
			return info.getCharacterURI();
		}
		
	}
	
	public void removeCharacterAgent(AID character) {
		
		m_characterInfo.remove(character);;
	}	

	/**
	 * Spawns a character agent
	 */
	private void spawnCharacterAgent() {
		// For safety, this prevents potential bugs in the code to continuously spawn new agents.
		if (m_characterInfo.size() > MAX_CHAR_AGENTS) return;
		
		// Spawn a new character! For now, new agentlauncher does it.
		// For future, make AgentLauncher also an agent?
		AgentLauncher al = new AgentLauncher();
		al.launchAgent(UniqueId.generateUniqueAgentName(
						"CHAR", "agent"), "vs.characteragent.BasicCharacterAgent");
		
	}
	
	private void tryCastCharacter(AID character, String characterURI) {
		// Send msg
		logger.info("REQUESTING character agent " + character + " to play character URI "
				+ characterURI);
		
		try {
			CastInformation info = m_characterInfo.get(character);
			if (info == null) {
				throw new CastException("Trying to cast a character for an agent which is not known to the Character Manager");
			}
			
			// Set to unavailable; we cannot ask it to become something else now.
			// Either the character refuses (then it stays unavailable)
			// or it accepts (then it becomes CASTED).
			info.setStatus(Status.UNAVAILABLE);
			m_castAdvertList.remove(characterURI);

			CharacterInfo ci = new CharacterInfo();
			ci.setIndividual(characterURI);
	
			ownerAgent.getAgent().addBehaviour(
					new InitiateRequestPlayCharacterBehaviour(ownerAgent.getAgent(), character, ci));
			
		} catch(CastException ce) {
			logger.severe("CastException: " + ce.getMessage());
		}
	};
}

