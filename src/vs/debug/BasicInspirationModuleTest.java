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

import java.util.Set;

import junit.framework.TestCase;
import vs.plotagent.BasicInspirationModule;
import vs.plotagent.inspiration.InspirationRule;

public class BasicInspirationModuleTest extends TestCase {

	BasicInspirationModule bim;
	
	@Override
	protected void setUp() throws Exception {
		super.setUp();

		bim = new BasicInspirationModule(null);
		
		assertTrue(true);
	}
	
	public void testBasicInspirationModule() {
		Set<InspirationRule> rules = bim.generateRules("file:knowledge/cases/PlopCase.trig");
		Set<InspirationRule> rules2 = bim.generateRules("file:knowledge/cases/PlopCase_m.trig");
		Set<InspirationRule> rules3 = bim.generateRules("file:knowledge/cases/cinderella_loses_shoe.trig");
		Set<InspirationRule> rules4 = bim.generateRules("file:knowledge/cases/impactOrganizeGarden.trig");
		Set<InspirationRule> rules5 = bim.generateRules("file:knowledge/cases/marrying_motivates_noDressPalace.trig");
		Set<InspirationRule> rules6 = bim.generateRules("file:knowledge/cases/cinderella_steps_on_twig.trig");
		
		
		System.out.println("PlopCase.trig			  : Produced " + rules.size() + " rules.");
		System.out.println("PlopCase_m.trig			  : Produced " + rules2.size() + " rules.");
		System.out.println("cinderella_loses_shoe.trig: Produced " + rules3.size() + " rules.");
		System.out.println("dressing_causes_happiness.trig: Produced " + rules4.size() + " rules.");
		System.out.println("marrying_moti...ingPalace.trig: Produced " + rules5.size() + " rules.");
		System.out.println("cinderella_steps_on...trig: Produced " + rules6.size() + " rules.");
		
		rules.addAll(rules2);
		rules.addAll(rules3);
		rules.addAll(rules4);
		rules.addAll(rules5);
		rules.addAll(rules6);
		System.out.println("Total:           produced " + rules.size() + " unique rules.");
		bim.writeRules(rules, "plopcase.pl");
	}

}
