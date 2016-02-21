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
package vs.debug;

import java.util.HashSet;
import java.util.Set;

import org.jpl7.Compound;
import org.jpl7.Term;
import junit.framework.TestCase;
import vs.plotagent.inspiration.Suggestion;

public class SuggestionTest extends TestCase {

	
	public void testSuggestion() {
		
		// Term jpl.Util.textToTerm( String sourcetext)
		
		Set<String> causers = new HashSet<String>();
		causers.add("cause");
		Suggestion s = new Suggestion(
				"'rule for generating E_loseShoe.1'",
				"individual",
				"type",
				causers,
				"'.'(rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#E_loseShoe.1', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#LoseItem', _36), '.'(rdf(i_sAction_10_cinderellaAgent, 'http://www.owl-ontologies.com/FabulaKnowledge.owl#phi_causes', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#E_loseShoe.1', _37), '.'(rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#E_loseShoe.1', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#agens', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cinderella', _38), '.'(rdf('http://www.owl-ontologies.com/FabulaKnowledge.owl#E_loseShoe.1', 'http://www.owl-ontologies.com/FabulaKnowledge.owl#patiens', 'http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#cloak', _39), []))))"
			);

		Term t = org.jpl7.Util.textToTerm(s.getBody());
		
		// Walk structure of term. It is a compound term consisting of a set of quads
		if (t instanceof Compound) {
			// ...which it is, in fact, it is a list.
			System.out.println(((Compound) t).listLength());
			Term[] quads = t.toTermArray();
			for (Term quad: quads) {
				if (quad instanceof Compound) {
					// ...which, again, it is. But now it is not a list.
					System.out.println("Quad:" + quad.name());
					
					for (Term arg: quad.args()) {

					//Term[] nodes = quad.toTermArray();
						if (arg.isAtom()) System.out.print("Atom: ");
						System.out.println(arg.toString());
					}
				}

			//}
			}
		}
		
		//System.out.println(t.toString());
		//System.out.println(t.debugString());
		
		assertTrue(true);
	}

}
