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
import java.util.logging.Logger;

import vs.debug.LogFactory;
import de.fuberlin.wiwiss.ng4j.NamedGraphSet;

public abstract class CaseTransformer {

	/** The logger **/
	protected Logger logger;
	
	/** Stores the named graph set containing the case fabula **/
	protected NamedGraphSet m_originalCase;	
	
	/**
	 * Constructor
	 * 
	 * @param ngs the Named Graph Set containing the case fabula.
	 */
	public CaseTransformer(NamedGraphSet ngs) {
		logger = LogFactory.getLogger(this);
		
		m_originalCase = ngs;
	}
	
	// TODO: add heuristics object and an adder

	public abstract Set<NamedGraphSet> transform();
}
