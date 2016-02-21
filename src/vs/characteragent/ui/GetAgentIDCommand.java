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
package vs.characteragent.ui;

import vs.rationalagent.RationalAgent;
import vs.rationalagent.ui.Command;
import vs.rationalagent.ui.RationalAgentGui;

public class GetAgentIDCommand extends Command
	{
		
		public GetAgentIDCommand( RationalAgent agent, RationalAgentGui agentGui )
		{
			super( agent, agentGui );
		}

		@Override
		public void execute()
		{
			super.execute();
				m_AgentGui.writeConsole("m_AgentID\n");
		}
		
		@Override
		public String getDescription()
		{
			return "Returns the AgentID that the character agent uses.\n" +
				"Usage:";
		}
		
		@Override
		public String getName()
		{
			return "GetAgentID";
		}
		
}
