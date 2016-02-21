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
import vs.plotagent.inspiration.BasicRuleBuilder;
import vs.plotagent.inspiration.InspirationRule;
import de.fuberlin.wiwiss.ng4j.NamedGraphSet;
import de.fuberlin.wiwiss.ng4j.impl.NamedGraphSetImpl;

public class BasicRuleBuilderTest extends TestCase {
	
	BasicRuleBuilder rb;

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		
		NamedGraphSet testSet = new NamedGraphSetImpl();
		testSet.read("file:knowledge/cases/plopcase.trig", "TRIG");
		
		rb = new BasicRuleBuilder(testSet);
	}
	
	public void testBasicRuleBuilder() {
		
		System.out.println("\nRules:");
		Set<InspirationRule> ir_set = rb.build();
		System.out.println(ir_set);
				
		assertTrue(true);
	}
	
	public void testRuleEquality() {
		
		System.out.println("\nRules:");
		Set<InspirationRule> ir_set = rb.build();
		Set<InspirationRule> ir_set2 = rb.build();
		
		System.out.println("Set 1:");
		for (InspirationRule ir: ir_set) {
			System.out.println(ir.hashCode());
		}

		System.out.println("Set 2:");
		for (InspirationRule ir: ir_set2) {
			System.out.println(ir.hashCode());
		}
		
		Set<InspirationRule> resSet = new HashSet<InspirationRule>();
		resSet.addAll(ir_set);
		resSet.addAll(ir_set2);
		System.out.println("Aggregated set contains " + resSet.size() + " items");
		
		//System.out.println(ir_set);
		
		assertTrue(resSet.size() == ir_set.size());
	}	
	
}
