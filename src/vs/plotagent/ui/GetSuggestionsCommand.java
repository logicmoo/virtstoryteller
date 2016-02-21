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
package vs.plotagent.ui;

import vs.plotagent.IPlotAgent;
import vs.plotagent.inspiration.Suggestion;
import vs.rationalagent.RationalAgent;
import vs.rationalagent.ui.Command;
import vs.rationalagent.ui.RationalAgentGui;

public class GetSuggestionsCommand extends Command {
	
	public GetSuggestionsCommand( RationalAgent agent, RationalAgentGui agentGui ) {
		super( agent, agentGui );
	}
	
	@Override
	public void execute()
	{
		super.execute();
		
		for (Suggestion suggestion: ((IPlotAgent)m_Agent).getInspirationModule().getSuggestions()) {
			m_AgentGui.writeConsole("Suggestion based on rule :", true);
			m_AgentGui.writeConsole(suggestion.toString(), true);
		}
				
	}
	
	@Override
	public String getDescription()
	{
		return "Retrieves suggestions given current fabula\n" +
			"Usage:\tgetsuggestions";
	}
	
	@Override
	public String getName()
	{
		return "getsuggestions";
	}	

}
