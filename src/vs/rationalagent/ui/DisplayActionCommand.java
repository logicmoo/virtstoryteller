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
 * Schedules an Action to the World Agent
 * 
 * @author swartjes
 * Created on 21-sep-2005
 */
public class DisplayActionCommand extends Command
{

	public DisplayActionCommand( RationalAgent agent, RationalAgentGui agentGui )
	{
		super( agent, agentGui );
	}

	@Override
	public void execute()
	{
		super.execute();
		
		if ( getArgumentCount() != 1 ) {
			m_AgentGui.writeConsole( "invalid number of arguments for command " + getName());
			m_AgentGui.writeConsole("Description of " + getName());
			m_AgentGui.writeConsole(getDescription());
		}
		else {
			// Retrieve from the actionDB all actions of this type
			/*Vector<Action> possibleActions = ((IWorldAgent)m_Agent).getActionManager().getActions(m_Agent.getKnowledgeManager().toSymbol(getArgument(0),Config.ACTION));
			
			// For each action
			if (!possibleActions.isEmpty()){
				for(Action a : possibleActions){			
					// Display action
					m_AgentGui.writeConsole(PrettyPrint.simplifyCNF(a.toString()));
				}		
			}
			else{
				m_AgentGui.writeConsole("No action found with name " + getArgument(0).toString());
			}*/
		}
		return;
	}

	@Override
	public String getDescription()
	{
		return "Displays the preconditions and effects of the specified action\n" +
				"Usage: displayaction <actionname>\n\n" + 
				"Warning: does not yet support <actiontype> as an argument...";
			//TODO: support <actiontype> as an argument.
	}
	
	@Override
	public String getName()
	{
		return "displayaction";
	}
}
