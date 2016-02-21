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

import java.util.Vector;

import vs.rationalagent.RationalAgent;

public abstract class Command {
	
	private	Vector<String>		m_arguments = null;
	private	String				m_allArguments = null;
	protected RationalAgent		m_Agent;
	protected RationalAgentGui	m_AgentGui;
	
	public Command( RationalAgent agent, RationalAgentGui agentGui )
	{
		m_Agent = agent;
		m_AgentGui = agentGui;
		m_arguments = new Vector<String>();
	}

	public void execute()
	{
		m_AgentGui.writeConsole( "command: " + getName(), false );
		for ( int i = 0; i < getArgumentCount(); i++ ) {
			m_AgentGui.writeConsole( " " + getArgument( i ), false );
		}
			
		m_AgentGui.writeConsole( "" );
	}

	public String getAllArguments()
	{
		return m_allArguments;
	}

	public String getArgument( int idx )
	{
		return m_arguments.get( idx );
	}

	public int getArgumentCount()
	{
		return m_arguments.size();
	}
	
	public abstract String getDescription();
	
	public abstract String getName();
	
	protected String parseArgument( String line )
	{
		if ( line.length() == 0 ) {
			return null;
		}
			
		char nexttoken = line.charAt( 0 );
		if ( nexttoken != '(' ) {
			// find next whitespace
			int wsidx = line.indexOf( ' ' );
			
			// no whitespace: line is a single word
			if ( wsidx == -1 ) {
				return line;
			}
			
			// return first word
			return line.substring( 0, wsidx );
		}
		
		// find right bracket
		int nestingdepth = 1;
		int b_idx;
		for ( b_idx = 1; b_idx < line.length(); b_idx++ ) {
			if ( nestingdepth == 0 ) {
				break;
			}
				
			char c = line.charAt( b_idx );
			if ( c == ')' ) {
				nestingdepth--;
			} else if ( c == '(' ) {
				nestingdepth++;
			}
		}
		
		return line.substring( 0, b_idx );
	}
	
	public void parseCommandLine( String arguments )
	{
		// split arguments
		// m_arguments = new Vector<String>();

		m_arguments.clear();
		m_allArguments = arguments.trim();
		
		while ( arguments.length() > 0 ) {
			// remove leading and trailing whitespaces
			arguments = arguments.trim();
			
			// take first word
			String arg = parseArgument( arguments );
			m_arguments.add( arg );
			
			// drop first word
			arguments = arguments.substring( arg.length());
		}
	}
	
}
