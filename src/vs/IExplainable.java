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

import java.util.Set;

import vs.communication.FabulaCausality;
import vs.communication.FabulaElement;

/**
 * Any class that implements the IExplainable interface is in more or less 
 * sense able to explain the decisions it has made.
 * 
 * For instance, a character agent receives perceptions, goes through an internal process,
 * and selects an action. The character agent is then asked: explain(action).
 * The character agent in turn has used its deliberative process to come to the decision,
 * and implements explain by asking the deliberative process to explain the action.
 * The deliberative process knows for which goal he has made a plan to come up with the action,
 * and returns this goal. 
 * 
 * @author swartjes
 *
 */
public interface IExplainable {
	
	/**
	 * Explain the motivations, the causalities or enablements that the interfaced class produced.
	 * 
	 * @return a set of causality declarations describing an explanation for fabula elements. 
	 */
	public Set<FabulaCausality> explainCausalities();
	
	/**
	 * Explain the fabula elements that the interfaced class produced
	 * 
	 * @return a set of fabula elements produced by the interfaced class 
	 */	
	public Set<FabulaElement> explainElements();
	

}
