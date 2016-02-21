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

public class UntellCommand extends Command
{
	
	public UntellCommand( RationalAgent agent, RationalAgentGui agentGui )
	{
		super( agent, agentGui );
	}

	@Override
	public void execute()
	{
		super.execute();
		
		if ( getArgumentCount() == 0 ) {
			m_AgentGui.writeConsole( "no argument specified" );
		} else {
			String assertion = getArgument( 0 );
			
			m_AgentGui.writeConsole( "Untelling: " + assertion + "... ", false);
			boolean success = false; 
			try {
				success = m_Agent.getKnowledgeManager().untellRDF(assertion);
			} catch (Exception e) {
				m_AgentGui.writeConsole( "failed" );
			}
			
			if (success) {
				m_AgentGui.writeConsole( "ok" );
			} else {
				m_AgentGui.writeConsole( "failed" );			
			}

			/*
			// parse string
			CNFSentence sent = m_Agent.knowledge().context().getTranslator().translate( assertion );
			if ( sent != null ) {
				m_Agent.knowledge().retract( sent.toString() );
				m_AgentGui.writeConsole( "ok" );
			} else {
				m_AgentGui.writeConsole( "parse error" );
			}*/
		}
	}
	
	@Override
	public String getDescription()
	{
		return "Untells a fact from the knowledge base\nUsage: untell <fact>\nExample: untell (answer question 42)";
	}
	
	@Override
	public String getName()
	{
		return "untell";
	}
}
