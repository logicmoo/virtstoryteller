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

import java.util.Set;

public class Suggestion {
	protected String m_ruleName;
	protected String m_individual;
	protected String m_type;
	protected Set<String> m_causers;
	protected String m_body;
	
	public Suggestion(String ruleName, String individual, String type, Set<String> causers, String body) {
		m_ruleName = ruleName;
		m_individual = individual;
		m_type = type;
		m_causers = causers;
		m_body = body;
	}
	
	public String getBody() {
		return m_body;
	}
	
	public Set<String> getCausers() {
		return m_causers;
	}
	
	public String getIndividual() {
		return m_individual;
	}	
	
	public String getRuleName() {
		return m_ruleName;
	}		
	
	public String getType() {
		return m_type;
	}	
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		
		sb.append("Suggestion based on rule ")
			.append(m_ruleName).append(":\n")
			.append("Individual: ").append(m_individual).append("\n")
			.append("Type:       ").append(m_type).append("\n")
			.append("Causers:    ").append(m_causers).append("\n")
			.append("Body: ").append(m_body);
		return sb.toString();
	}
}
