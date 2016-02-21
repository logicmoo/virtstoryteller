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

import jade.core.Agent;
import jade.core.behaviours.SimpleBehaviour;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;

import java.util.logging.Logger;

import vs.debug.LogFactory;

/**
 * @deprecated
ReceiveBehaviour:   a more user friendly ReceiverBehaviour

Creation: new ReceiveBehaviour(Agent, Timeout (or -1), MessageTemplate )

	- terminates when 1) desired message is received OR timeout expires
    - on termination, handle(msg) is called
                   ( J.Vaucher sept. 7 2003 )
                   http://www.iro.umontreal.ca/~vaucher/Agents/Jade/primer6.html                   
**/
@Deprecated
public abstract class ReceiveBehaviour extends SimpleBehaviour {
	
	private MessageTemplate template;
	private long    timeOut, 
	                wakeupTime;
	private boolean finished;
	  
	private ACLMessage msg;
	private Logger logger;
	
	/** Constructor
	 * 
	 * @param a The agent of this Behaviour
	 * @param millis timeout (-1 if no timeout)
	 * @param mt Message Template for message to receive
	 */
	public ReceiveBehaviour(Agent a, int millis, MessageTemplate mt) {
		super(a);
		timeOut = millis;
		template = mt;
		
		logger = LogFactory.getLogger(this);
	}
  
	/** Called when behaviour is active **/
	@Override
	public void action() {
		if(template == null) {
			msg = myAgent.receive();
		} else {
			msg = myAgent.receive(template);
		}
		if( msg != null) {
			finished = handle( msg );
			return;
		}

		long dt = wakeupTime - System.currentTimeMillis();
		if ( dt > 0 ) {
			block(dt);
		} else {
			finished = handle( msg );
		}
	}
  
	/** Action is done when finished = true **/
	@Override
	public boolean done () {
		if (finished) {
			 logger.info("DONE " + this.getBehaviourName());
		}
		return finished;
	}

	/**
	 * Gets the message
	 * @return the ACLMessage
	 */
	public ACLMessage getMessage() { 
		return msg; 
	}
	
	/** Defines how to handle this message.
	 * @param msg the received ACL message, or null if time out
	 * @return whether the ACL message was handled. I.e. if it was put back, 
	 * this method should return false.
	 **/
	public abstract boolean handle( ACLMessage msg);

	/** Called when behaviour starts **/
	@Override
	public void onStart() {
		wakeupTime = (timeOut<0 ? Long.MAX_VALUE : System.currentTimeMillis() + timeOut);
	} 
	

	@Override
	public void reset() {
		msg = null;
		finished = false;
		super.reset();
  	}
  	
	public void reset(int dt) {
		timeOut= dt;
		reset();
  	}

}
