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
package vs.fabula;

import java.io.File;

import jpl.Atom;
import vs.knowledge.PrologKB;

public class PrologFabulaKnowledgeBase extends FabulaKnowledgeBase {

	public PrologFabulaKnowledgeBase() {
	}

	@Override
	public boolean addQuad(Object graph, Object elem1, Object rel, Object elem2) {

		if (graph == null || elem1 == null || rel == null || elem2 == null) {
			return false;
		}
		
		if (elem2.toString().charAt(0) == '_') {
			// Object is a don't care variable; do not use in fabula
			return false;
		}		

		StringBuilder termBuilder = new StringBuilder();
		termBuilder.append('(');
		Atom a = new Atom(elem1.toString());
		termBuilder.append(a);		
		termBuilder.append(',');
		a = new Atom(rel.toString());
		termBuilder.append(a);
		termBuilder.append(',');
		a = new Atom(elem2.toString());
		termBuilder.append(a);
		termBuilder.append(",");
		a = new Atom(graph.toString());
		termBuilder.append(a);
		termBuilder.append(')');
		
		logger.fine("Adding term:\n" + termBuilder.toString());

		return PrologKB.getInstance().tellRDF(termBuilder.toString());

	}

	@Override
	// FIXME: Check whether string is a literal, then it should not be
	// encapsulated with '''s!! Maybe use addTriple(Triple t) anyway!
	public boolean addTriple(Object elem1, Object rel, Object elem2) {

		if (elem1 == null || rel == null || elem2 == null) {
			return false;
		}
		
		if (elem2.toString().charAt(0) == '_') {
			// Object is a don't care variable; do not use in fabula
			return false;
		}

		StringBuilder termBuilder = new StringBuilder();
		termBuilder.append("(");
		Atom a = new Atom(elem1.toString());
		termBuilder.append(a);
		// termBuilder.append('\'').append(elem1).append('\'');
		termBuilder.append(',');
		a = new Atom(rel.toString());
		termBuilder.append(a);
		// termBuilder.append('\'').append(rel).append('\'');
		termBuilder.append(',');
		a = new Atom(elem2.toString());
		termBuilder.append(a);
		// termBuilder.append('\'').append(elem2).append('\'');
		termBuilder.append(")");

		logger.fine("Adding term:\n" + termBuilder.toString());

		return PrologKB.getInstance().tellRDF(termBuilder.toString());

	}

	@Override
	public void saveFabula(File filename, String language) {

		// do nothing
	}

}
