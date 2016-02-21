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

import jade.content.lang.Codec.CodecException;
import jade.content.onto.OntologyException;
import jade.core.AID;
import jade.core.Agent;
import jade.core.behaviours.OneShotBehaviour;
import jade.lang.acl.ACLMessage;
import vs.communication.Trace;
import vs.communication.TraceInformation;
import vs.rationalagent.IRationalAgent;
import vs.rationalagent.RationalAgent;

/**
 * A one shot behaviour that registers an agent with the DF
 **/

public class TraceBehaviour extends OneShotBehaviour
{
	/////////////////////////////////////////////
	// ATTRIBUTES
	/////////////////////////////////////////////
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 5395094810181986387L;
	private IRationalAgent m_rationalAgent;
	private int m_verbosity;
	private int m_depth;
	private String m_message;
	//private String			m_serviceName;
	
	/////////////////////////////////////////////
	// CONSTRUCTORS AND INSTANTIATION
	/////////////////////////////////////////////
		
	public TraceBehaviour(Agent a, int verbosity, int depth, String message) //, String serviceName)
	{
		super(a);
		m_rationalAgent = (IRationalAgent)a;
		m_verbosity = verbosity;
		m_depth = depth;
		m_message = message;
		//m_serviceName = serviceName;
	}

	/////////////////////////////////////////////
	// METHODS
	/////////////////////////////////////////////
	
	@Override
	public void action()
	{
		AID tracerAID = m_rationalAgent.getTracer();
		
		if ( tracerAID == null ) {
			AID foundTracer = findTracer();
			if (foundTracer == null) {
				return;
			} else {
				m_rationalAgent.setTracer(foundTracer);
				tracerAID = foundTracer;
			}
		}
			
		// Assumption from here: tracerAID is an existing Tracer AID.
		
		// argument 1: agent's name to send information to
		ACLMessage msg = new ACLMessage( ACLMessage.INFORM );
		msg.addReceiver( tracerAID );
		msg.setLanguage( m_rationalAgent.getCodec().getName());
		msg.setOntology( m_rationalAgent.getOntology().getName());
		
		TraceInformation traceinfo = new TraceInformation();
		traceinfo.setVerbosity( new Integer( m_verbosity ));
		traceinfo.setTraceDepth( new Integer( m_depth ));
		traceinfo.setMessage( m_message );
		
		Trace doTrace = new Trace();
		doTrace.setTraceInformation(traceinfo);
		
		try {
			myAgent.getContentManager().fillContent( msg, doTrace );
			myAgent.send( msg );
		}
		catch( CodecException ce ) {
			ce.printStackTrace();
		}
		catch( OntologyException oe ) {
			oe.printStackTrace();
		}
		
	}
		
	/** 
	 * Attempts to find Tracer agent
	 * @return AID of Tracer agent
	 */
	public AID findTracer() {
    	// Try to find Tracer
		AID foundTracer = null;
    	AID[] tracerServices = m_rationalAgent.findServiceType( RationalAgent.TRACER_SERVICE);
    	if ( tracerServices != null && tracerServices.length > 0 ) {
    		foundTracer = tracerServices[ 0 ];
    	}
    	
    	return foundTracer;
	}
}