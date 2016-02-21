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
package vs.rationalagent;

import vs.IAgent;
import vs.knowledge.PrologKB;

/**
 * Rational interface, for any object that makes use of a publicly accessible Knowledge manager
 * 
 * @author swartjes
 * Created on 21-jul-2005
 */
public interface IRationalAgent extends IAgent {

	/** Retrieve the knowledge manager
	 * @deprecated - use PrologKB.getInstance() */
	@Deprecated
	public PrologKB getKnowledgeManager();
	
	/** 
	 * Gets the world time (in time steps)
	 * @return the time
	 */
	public int getTime();
	
	public void writeGui(String msg);

}
