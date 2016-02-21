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
package vs.plotagent.inspiration;

import vs.communication.FabulaElement;

public class OperationalizedSuggestion {
	
	protected Suggestion m_suggestion;
	protected FabulaElement m_operationalizedElement;
	
	public OperationalizedSuggestion(Suggestion sugg) {
		m_suggestion = sugg;
	}
	
	public FabulaElement getOperationalizedElement() {
		return m_operationalizedElement;
	}
	public Suggestion getSuggestion() {
		return m_suggestion;
	}

	public void setOperationalizedElement(FabulaElement element) {
		m_operationalizedElement = element;
	}
	
	public void setSuggestion(Suggestion m_suggestion) {
		this.m_suggestion = m_suggestion;
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(m_suggestion.toString()).append('\n');
		sb.append(m_operationalizedElement.toString()).append(":\n");
		sb.append("individual: ").append(m_operationalizedElement.getIndividual()).append('\n');
		sb.append("type:       ").append(m_operationalizedElement.getType()).append('\n');
		sb.append("character:  ").append(m_operationalizedElement.getCharacter()).append('\n');
		return sb.toString();
	}
	
	

}
