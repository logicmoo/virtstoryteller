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

import junit.framework.TestCase;
import vs.utils.Chooser;

public class ChooserTest extends TestCase {

	public void testChooseEmpty() {
		Object choice = Chooser.randomChoice(new HashSet<Object>());
		assertTrue(choice == null);
	}
	
	public void testChooseNull() {
		Object choice = Chooser.randomChoice(null);
		assertTrue(choice == null);
	}
	
	public void testChooser() {
		Set<String> testSet = new HashSet<String>();
		
		testSet.add("Aap");
		testSet.add("Noot");
		testSet.add("Mies");
		
		String choice = Chooser.randomChoice(testSet);
		System.out.println("Choice: " + choice);
		
		assertTrue(choice != null);
	}	
}
