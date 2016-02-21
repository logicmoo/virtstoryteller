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
public class ValidateActionCommand extends Command
{

	public ValidateActionCommand( RationalAgent agent, RationalAgentGui agentGui )
	{
		super( agent, agentGui );
	}

	@Override
	public void execute()
	{
		super.execute();
		
		if ( getArgumentCount() != 5 ) {
			m_AgentGui.writeConsole( "invalid number of arguments for command " + getName());
			m_AgentGui.writeConsole("Description of " + getName());
			m_AgentGui.writeConsole(getDescription());
		}
		else {
			// What this does: retrieve from the actionDB of the World Agent the correct action.
			
			// TODO
			
			
			/*Action a = ((BasicWorldAgent)m_Agent).getActionManager().getAction(getArgument(0),Config.ACTION);
			
			if(a != null){
				Map<String,Object> m = new HashMap<String,Object>();
				
				m.put("AGENS", ((BasicWorldAgent)m_Agent).getKnowledgeManager().toSymbol(getArgument(1)));
				m.put("PATIENS", ((BasicWorldAgent)m_Agent).getKnowledgeManager().toSymbol(getArgument(2)));
				m.put("TARGET",  ((BasicWorldAgent)m_Agent).getKnowledgeManager().toSymbol(getArgument(3)));
				m.put("INSTRUMENT", ((BasicWorldAgent)m_Agent).getKnowledgeManager().toSymbol(getArgument(4)));
			
				
				// Argument binding for the AgentID for amalia. The AgentID is not an action argument, but is always
				// passed between an actor agent and the Story World. This AgentID determines which object the
				// Character Agent is controlling. This is a hack because the Character Agent is not yet
				// implemented
				m.put("AGENTID", ((IWorldAgent)m_Agent).getKnowledgeManager().toSymbol("AmaliaAgent", Config.SWI));

				a.setVariables(m);
				
				m_AgentGui.writeConsole("Checking preconditions of action: \n" + PrettyPrint.simplifyCNF(a.toString()));
				
				// validatePreconditions
				Preconditions invalidPreconditions = ((BasicWorldAgent)m_Agent).getActionManager().validatePreconditions(a);
				if (invalidPreconditions.isEmpty()){
					m_AgentGui.writeConsole("All preconditions of this action are valid");
				}
				else{
					m_AgentGui.writeConsole("The following preconditions are invalid:\n" + PrettyPrint.simplifyCNF(invalidPreconditions.toString()));
				}
			}
			else
				m_AgentGui.writeConsole(getArgument(0) + " is not a valid action of the action database.");
				*/			
		}
	}

	@Override
	public String getDescription()
	{
		return "Validates an action: Checks the preconditions of an action. If they\n" +
				"are invalid, show the invalid clauses. \n \nUseage:" +
			"validateaction <actionname> <CONTENTS>\n" +
			"example: validateaction blablabla";
	}
	
	@Override
	public String getName()
	{
		return "validateaction";
	}
}
