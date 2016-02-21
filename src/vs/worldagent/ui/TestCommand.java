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

/**
 * Checks if an action is valid. Outputs why an action is valid or not.
 * 
 * @author uijlings
 * Created on 7-dec-2005
 */
public class TestCommand extends Command
{

	public TestCommand( RationalAgent agent, RationalAgentGui agentGui )
	{
		super( agent, agentGui );
	}

	@Override
	public void execute()
	{
		super.execute();
		
/*		Symbol actionType = ((IWorldAgent)m_Agent).getKnowledgeManager().toSymbol("Run", Config.ACTION);
		
		Vector<Action> allActions = ((IWorldAgent) m_Agent).getActionManager().getActions(actionType);
		
		for(Action a : allActions)
			m_AgentGui.writeConsole(PrettyPrint.simplifyCNF(a.toString()));*/
	}

	@Override
	public String getDescription()
	{
		return "Does a specific test. Useful for debugging.";
	}
	
	@Override
	public String getName()
	{
		return "test";
	}
}
