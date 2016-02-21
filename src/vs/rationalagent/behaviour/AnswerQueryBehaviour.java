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
package vs.rationalagent.behaviour;

import jade.core.behaviours.OneShotBehaviour;

import java.util.Hashtable;
import java.util.Map;

import vs.rationalagent.RationalAgent;
import vs.rationalagent.ui.RationalAgentGui;


public class AnswerQueryBehaviour extends OneShotBehaviour
{
	/////////////////////////////////////////////
	// ATTRIBUTES
	/////////////////////////////////////////////

	/**
	 * 
	 */
	private static final long serialVersionUID = 1536550106438684912L;
	//private RDFReasoningContext		m_context;
	private RationalAgentGui		m_gui;
	private Map[] m_answers;

	/////////////////////////////////////////////
	// CONSTRUCTORS AND INSTANTIATION
	/////////////////////////////////////////////

	public AnswerQueryBehaviour(RationalAgent ra, Hashtable[] answers, RationalAgentGui gui)
	{
		super(ra);
		//m_context = ra.knowledge().context();
		m_answers = answers;
		m_gui = gui;
		
		//m_printer = new CNFPrinter();
		//m_printer.setNamespaceMap( m_context.getKifParser().getNamespaceMap());
	}

	/////////////////////////////////////////////
	// ACTION AND DONE
	/////////////////////////////////////////////

	public AnswerQueryBehaviour(RationalAgent m_Agent, Map[] answers,
			RationalAgentGui m_AgentGui) {
		super(m_Agent);
		//m_context = ra.knowledge().context();
		m_answers = answers;
		m_gui = m_AgentGui;
	}

	@Override
	public void action()
	{
		if ( m_answers != null ) {
			int idx = 1;
			for (Map bindings: m_answers) {
				if ( !bindings.isEmpty() ) {
					m_gui.writeConsole( "Answer #" + idx + ":" );
					idx++;
					displayAnswer( bindings );
				}
				
			}
/*			for ( Iterator it = m_answers.iterator(); it.hasNext(); ) {
				Map<Variable,Object> bindings = it.next();
				if ( !bindings.isEmpty() ) {
					m_gui.writeConsole( "Answer #" + idx + ":" );
					idx++;
					displayAnswer( bindings );
					m_gui.writeConsole( "" );
				}
			}*/
		} else {
		}
	}

	/////////////////////////////////////////////
	// METHODS
	/////////////////////////////////////////////
	
	protected void displayAnswer(Map bindings) {
		m_gui.writeConsole(bindings.toString());
		
	}

	protected void displayAnswer( Hashtable bindings )
	{
		m_gui.writeConsole(bindings.toString());
		/*for (Map.Entry<Variable,Object> ent: bindings.entrySet()) {
			m_gui.writeConsole("   "+ ent.getKey() + " = " + ent.getValue());
		}*/
		/*for (Iterator it = bindings.entrySet().iterator(); it.hasNext();) {
		    Map.Entry ent = (Map.Entry)it.next();
		    m_gui.writeConsole("   "+ ent.getKey() + " = " + ent.getValue());
		}*/		
	}
}