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

import vs.rationalagent.RationalAgent;
import vs.rationalagent.ui.Command;
import vs.rationalagent.ui.RationalAgentGui;
import vs.worldagent.IWorldAgent;

/**
 * Pulses the time of the world
 * 
 * @author swartjes
 * Created on 21-sep-2005
 */
public class PulseOnCommand extends Command
{

	public PulseOnCommand( RationalAgent agent, RationalAgentGui agentGui )
	{
		super( agent, agentGui );
	}

	@Override
	public void execute()
	{
		super.execute();
		
		// Check if there are any actions in the ActionSchedule. If so: show the action list.		
		if (((IWorldAgent) m_Agent).getOperatorScheduler().getScheduledOperators().isEmpty() == false){
			m_AgentGui.writeConsole( ((IWorldAgent)m_Agent).getOperatorScheduler().toString());
		} else {
			m_AgentGui.writeConsole("No actions in action schedule. Pulse is not called.\n");
		}
		
		// Call pulse() untill there are no more scheduled actions.
		while (((IWorldAgent) m_Agent).getOperatorScheduler().getScheduledOperators().isEmpty() == false){
			((IWorldAgent)m_Agent).pulse(m_Agent.getTime()+1); // there are still scheduled actions => call pulse().
		}
		
		// Display the empty action list with corresponding timestep.
		m_AgentGui.writeConsole( ((IWorldAgent)m_Agent).getOperatorScheduler().toString());
	}

	@Override
	public String getDescription()
	{
		return "increases time step until no more actions are scheduled\ncareful: desynchronizes simulation state. Use for debugging only.\n";
	}
	
	@Override
	public String getName()
	{
		return "pulseOn";
	}
}
