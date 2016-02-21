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

public class HelpCommand extends Command
{

	public HelpCommand( RationalAgent agent, RationalAgentGui agentGui )
	{
		super( agent, agentGui );
	}

	@Override
	public void execute()
	{
		super.execute();
		
		if ( getArgumentCount() == 0 ) {
			m_AgentGui.writeConsole( "available commands: " );
			m_AgentGui.showRegisteredCommands();
			m_AgentGui.writeConsole( "" );
		} else if ( getArgumentCount() == 1 ) {
			String cmd_name = getArgument( 0 );
			
			if ( !m_AgentGui.showCommand( cmd_name )) {
				m_AgentGui.writeConsole( "unknown command: " + cmd_name );
			}
				
		} else {
		}
	}

	@Override
	public String getDescription()
	{
		return "help <cmd>\tgives a description of command cmd\n"+
			   "help \tgives an overview of available commands";
	}
	
	@Override
	public String getName()
	{
		return "help";
	}
}
