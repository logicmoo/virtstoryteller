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
package vs;

import jade.content.lang.Codec;
import jade.content.onto.Ontology;
import jade.core.AID;
import jade.core.Agent;
import vs.rationalagent.StoryAgentEventListener;
import vs.rationalagent.ui.StoryAgentEvent;

/**
 * IAgent interface, for any object that makes use of a publicly accessible Knowledge manager
 * 
 * @author swartjes
 * Created on 21-jul-2005
 */
public interface IAgent {

	public void addEventListener(StoryAgentEventListener listener);
	
	/**
	 * Searches the JADE platform for services of the given type
	 * @return A list of all agents offering this service.
	**/
	public AID[] findServiceType( String type ); 
	
	public void fireEvent(StoryAgentEvent e);
	
	/** Generates a unique conversation ID
	 * See http://www.iro.umontreal.ca/~vaucher/Agents/Jade/primer6.html
	 * @return a unique conversation ID
	 * **/
	public String genCID();	
	
	/** Get the agent itself **/
	public Agent getAgent();
	
	/**
	 * @return The (@link Codec) used by this agent
	**/	
	public Codec getCodec();
	
	/** Get local name of agent. This is already implemented
	 * as a final method of jade.core.Agent. This function is only
	 * used to initialize logger**/
	public String getLocalName();
	/**
	 * @return The (@link Ontology) used by this agent
	**/	
	public Ontology getOntology();

	/**
	 * Getter for Tracer agent
	 * @return AID of Tracer agent
	 */
	public AID getTracer();
	
	public void removeEventListener(StoryAgentEventListener listener);
	
	/**
	 * Setter for Tracer agent
	 * @param tracer AID of Tracer agent
	 */
	public void setTracer(AID tracer);
	/**
	 * Send a message to the Tracer agent if the tracer-service has been found in
	 * the yellow pages.
	 * 
	 * @param verbosity the importance of the message
	 * @param message the message itself
	 */
	public void trace( int verbosity, int level, String message );
	public void trace( int verbosity, String message );
}
