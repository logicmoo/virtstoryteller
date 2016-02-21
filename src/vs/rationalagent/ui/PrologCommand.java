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

import java.util.Hashtable;
import java.util.Map;

import org.jpl7.PrologException;
import org.jpl7.Query;

import vs.rationalagent.RationalAgent;
import vs.rationalagent.behaviour.AnswerQueryBehaviour;

public class PrologCommand extends Command
{
	
	public PrologCommand( RationalAgent agent, RationalAgentGui agentGui )
	{
		super( agent, agentGui );
	}

	@Override
	public void execute()
	{
		super.execute();
		
		if ( getArgumentCount() == 0 ) {
			m_AgentGui.writeConsole( "prolog: no query specified" );
		} else {
			String prologCommand = getAllArguments();
			
			m_AgentGui.writeConsole( "Prologging: " + prologCommand);
			
			try {
				Query q = new Query(prologCommand);
				
				Map[] answers = q.allSolutions();
				m_Agent.addBehaviour( new AnswerQueryBehaviour(m_Agent, answers, m_AgentGui ));
				m_AgentGui.writeConsole(answers.length + " answers.");
			} catch(PrologException pe) {
				pe.printStackTrace();
			}
		}
	}
	
	@Override
	public String getDescription()
	{
		return "Queries the knowledge base\n" +
			"Usage:\tprolog <fact>\nExample:\tprolog member(a, [a,b,c]).";
	}
	
	@Override
	public String getName()
	{
		return "prolog";
	}
}