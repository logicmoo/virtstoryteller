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
package vs.rationalagent.ui;

import vs.rationalagent.RationalAgent;

/**
 * @deprecated
 * @author swartjes
 *
 */
@Deprecated
public class ListFactsCommand extends Command
{
	
	public ListFactsCommand( RationalAgent agent, RationalAgentGui agentGui )
	{
		super( agent, agentGui );
	}

	@Override
	public void execute()
	{
		super.execute();
		// Get facts
		String facts = "This function is obsolete";
		
		// PrettyPrint facts
		m_AgentGui.writeConsole( facts);
	}
	
	@Override
	public String getDescription()
	{
		return "Shows all facts asserted to the agent's knowledge base";
	}
	
	@Override
	public String getName()
	{
		return "facts";
	}
}
