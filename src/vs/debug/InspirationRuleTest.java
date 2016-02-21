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
import vs.plotagent.inspiration.InspirationRule;

import com.hp.hpl.jena.graph.Node;

import de.fuberlin.wiwiss.ng4j.Quad;

public class InspirationRuleTest extends TestCase {
	
	Quad q1, q2, q3, q4;
	
    @Override
	protected void setUp() {
        q1 = new Quad(Node.createURI("http://www.bizer.de/InformationAboutRichard"),
                Node.createURI("http://richard.cyganiak.de/foaf.rdf#RichardCyganiak"),
                Node.createURI("http://xmlns.com/foaf/0.1/mbox") ,
                Node.createURI("mailto:richard@cyganiak.de"));
        q2 = new Quad(Node.createURI("http://www.bizer.de/InformationAboutIvo"),
                Node.createURI("http://richard.cyganiak.de/foaf.rdf#IvoSwartjes"),
                Node.createURI("http://xmlns.com/foaf/0.1/mbox") ,
                Node.createURI("mailto:ivo@swartjes.nl"));
        
        q3 = new Quad(Node.createURI("http://www.bizer.de/NameInfo"),
                Node.createURI("http://richard.cyganiak.de/foaf.rdf#person.12"),
                Node.createURI("http://richard.de/hasName") ,
                Node.createURI("http://richard.cyganiak.de/foaf.rdf#IvoSwartjes"));
        
        q4 = new Quad(Node.createURI("http://www.bizer.de/NameInfo"),
                Node.createURI("http://richard.cyganiak.de/foaf.rdf#person.14"),
                Node.createURI("http://richard.de/hasName") ,
                Node.createURI("http://richard.cyganiak.de/foaf.rdf#RichardCyganiak"));        
    }
    
    public void testRule() {
        
    	Set<Node> causers = new HashSet<Node>();
    	causers.add(Node.createURI("http://richard.cyganiak.de/foaf.rdf#IvoSwartjes"));
    	InspirationRule ir = new InspirationRule(
    			"testRule", 
    			Node.createURI("http://richard.cyganiak.de/foaf.rdf#person.12"),
    			Node.createURI("http://richard.cyganiak.de/foaf.rdf#Person"),
    			causers,
    			new HashSet<Node>());
    	ir.addAntecedent(q3);
    	ir.addAntecedent(q4);
    	
    	ir.addConsequent(q1);
    	ir.addConsequent(q2);    	
    	
    	String result= ir.toString();
    	System.out.println(result);
    	
        assertTrue(true);
    }
}
