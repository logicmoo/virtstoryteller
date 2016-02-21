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
package vs.worldagent.ui;

import vs.communication.StoryAction;
import vs.rationalagent.RationalAgent;
import vs.rationalagent.ui.Command;
import vs.rationalagent.ui.RationalAgentGui;
import vs.worldagent.IWorldAgent;

/**
 * Schedules an Action to the World Agent
 * 
 * @author swartjes
 * Created on 21-sep-2005
 */
public class ScheduleActionCommand extends Command {

	public ScheduleActionCommand(RationalAgent agent, RationalAgentGui agentGui) {
		super(agent, agentGui);
	}

	@Override
	public void execute() {
		super.execute();

		if (getArgumentCount() != 5) {
			m_AgentGui.writeConsole("invalid number of arguments for command "
					+ getName());
			m_AgentGui.writeConsole("Description of " + getName());
			m_AgentGui.writeConsole(getDescription());
		} else {
			// What this does: retrieve from the actionDB of the World Agent the correct action.
			StoryAction a = new StoryAction();
			a.setIndividual(getArgument(0));
			a.setAgens(getArgument(1));
			a.setPatiens(getArgument(2));
			a.setTarget(getArgument(3));
			a.setInstrument(getArgument(4));
			((IWorldAgent) m_Agent).getOperatorScheduler().schedule(a);

			/*if(a != null){
				// Bind the action arguments to their corresponding variables.
				Map<String,Object> m = new HashMap<String,Object>();
				
				m.put("AGENS", ((IWorldAgent)m_Agent).getKnowledgeManager().toSymbol(getArgument(1), Config.SWI));
				m.put("PATIENS", ((IWorldAgent)m_Agent).getKnowledgeManager().toSymbol(getArgument(2), Config.SWI));
				m.put("TARGET",  ((IWorldAgent)m_Agent).getKnowledgeManager().toSymbol(getArgument(3), Config.SWI));
				m.put("INSTRUMENT", ((IWorldAgent)m_Agent).getKnowledgeManager().toSymbol(getArgument(4), Config.SWI));
			
			
				// Argument binding for the AgentID for amalia. The AgentID is not an action argument, but is always
				// passed between an actor agent and the Story World. This AgentID determines which object the
				// Character Agent is controlling. This is a hack because the Character Agent is not yet
				// implemented
				m.put("AGENTID", ((IWorldAgent)m_Agent).getKnowledgeManager().toSymbol("AmaliaAgent", Config.SWI));

				a.setVariables(m);
				
				m_AgentGui.writeConsole("Trying to schedule action: " + PrettyPrint.simplifyCNF(a.toString()));
				
				// Try to schedule the action.
				((IWorldAgent)m_Agent).getActionScheduler().schedule(a);
			}
			else
				m_AgentGui.writeConsole(getArgument(0) + " is not a valid action of the action database.");
				*/
		}
	}

	@Override
	public String getDescription() {
		return "schedule an action:\n"
				+ "scheduleaction <actionname> <CONTENT>\n"
				+ "example: scheduleaction pickup.235 blablablablabala";
	}

	@Override
	public String getName() {
		return "scheduleaction";
	}
}
