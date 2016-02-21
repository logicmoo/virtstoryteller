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
package vs.plotagent;

import vs.IAgentModule;
import vs.IExplainable;
import vs.communication.OperatorResult;
import vs.communication.WorldChange;

/**
 * A PerceptionManager is responsible for translating world changes and operator results 
 * into perceptions for characters. It decides which character should receive which perceptions
 * based on its own intelligence.
 * 
 * @author swartjes
 *
 */
public interface IPerceptionManager extends IAgentModule, IExplainable {

	/**
	 * Sends new perceptions and setting elements to the characters
	 */
	public void informCharacters();

	/**
	 * Register operator results
	 * 
	 * @param or the Operator Result to register
	 */
	public void registerOperatorResult(OperatorResult or);

}
