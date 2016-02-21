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
package vs.tracer;

//import vs.agentontology.*;
import jade.content.ContentElement;
import jade.content.lang.Codec.CodecException;
import jade.content.onto.OntologyException;
import jade.core.behaviours.CyclicBehaviour;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import vs.communication.Trace;
import vs.communication.TraceInformation;
/**
 * A cyclic behaviour that receives messages from other agents
 */

public class TracerReceiveBehaviour extends CyclicBehaviour
{
	/////////////////////////////////////////////
	// ATTRIBUTES
	/////////////////////////////////////////////

	/////////////////////////////////////////////
	// CONSTRUCTORS AND INSTANTIATION
	/////////////////////////////////////////////
		
	/**
	 * 
	 */
	private static final long serialVersionUID = -1750283467347785195L;

	public TracerReceiveBehaviour(TracerAgent a)
	{
		super(a);
	}

	/////////////////////////////////////////////
	// METHODS
	/////////////////////////////////////////////	

	@Override
	public void action()
	{
		TracerAgent tracer = (TracerAgent) myAgent;
		// we're only interested in messages we can comprehend
		MessageTemplate mt = MessageTemplate.and(
			MessageTemplate.MatchLanguage( tracer.getCodec().getName()),
			MessageTemplate.MatchOntology( tracer.getOntology().getName()));
			
		// receive a single message (non-blocking)
		ACLMessage msg = tracer.receive( mt );
		
		if (msg != null) {
			try {
				ContentElement ce = null;
				ce = tracer.getContentManager().extractContent( msg );
				
				// process trace-messages
				if ( ce instanceof Trace ) {
					TraceInformation ti = ((Trace) ce).getTraceInformation();
					tracer.handleTrace( msg.getSender(), 
						ti.getVerbosity(),
						ti.getTraceDepth(),
						ti.getMessage());
				}
			}
			catch ( CodecException ce ) {
				ce.printStackTrace();
			}
			catch ( OntologyException oe ) {
				oe.printStackTrace();
			}
			
		} else {
			//Block this behaviour until a message arrives
			block();
			return;
		}
	}
}
