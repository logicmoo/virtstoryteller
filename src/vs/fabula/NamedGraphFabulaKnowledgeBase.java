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
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import com.hp.hpl.jena.graph.Node;
import com.hp.hpl.jena.graph.Triple;
import com.hp.hpl.jena.rdf.model.Literal;

import de.fuberlin.wiwiss.ng4j.NamedGraphSet;
import de.fuberlin.wiwiss.ng4j.Quad;
import de.fuberlin.wiwiss.ng4j.impl.NamedGraphSetImpl;

public class NamedGraphFabulaKnowledgeBase extends FabulaKnowledgeBase {
	
	protected NamedGraphSet fabula;
	
	
	public NamedGraphFabulaKnowledgeBase() {
		super();
			
		fabula = new NamedGraphSetImpl();
	
		//fabula.asJenaModel("").setNsPrefixes(Config.namespaceMap);
		
		fabula.createGraph(BasicFabulaBuilder.defaultGraphName);
		
	}
	
	@Override
	public boolean addQuad(
			Object graph, 
			Object elem1, 
			Object rel, 
			Object elem2) {
		
		if (graph == null || elem1 == null || rel == null || elem2 == null) {
			return false;
		}
		
		if (elem2.toString().charAt(0) == '_') {
			// Object is a don't care variable; do not use in fabula
			return false;
		}		

		Quad q = new Quad(
				makeNode(graph),
				makeNode(elem1), 
				makeNode(rel), 
				makeNode(elem2));
		logger.fine("Adding quad to fabula: " + q.toString());
		
		addQuad(q);

		return true;		
	}
	
	protected boolean addQuad(Quad q) {
		
		logger.fine("Adding quad to fabula: " + q.toString());		
		fabula.addQuad(q);
		
		return true;
	}	
	
	
	@Override
	public boolean addTriple(
			Object elem1, 
			Object rel, 
			Object elem2)  {
		
		if (elem1 == null || rel == null || elem2 == null) {
			return false;
		}
		
		if (elem2.toString().charAt(0) == '_') {
			// Object is a don't care variable; do not use in fabula
			return false;
		}		

		Triple t = new Triple(
				makeNode(elem1), 
				makeNode(rel), 
				makeNode(elem2));
		logger.fine("Adding triple to fabula: " + t.toString());	
		
		addTriple(t);

		return true;
	}
	
	protected boolean addTriple(Triple t) {
		logger.fine("Adding triple to fabula: " + t.toString());		
		fabula.getGraph(BasicFabulaBuilder.defaultGraphName).add(t);
		return true;
	}	
	
	protected boolean addTriple(Triple t, String graphName) {
		logger.fine("Adding triple to fabula: " + t.toString());		
		fabula.getGraph(graphName).add(t);
		return true;
	}
	
	public NamedGraphSet getNamedGraphSet() {
		return fabula;
	}
	
	public Node makeNode(Object src) {
		
		if (src instanceof Literal) {
			return ((Literal)src).asNode();
		} else {
			return Node.createURI(src.toString());
		}
	}
	
	@Override
	public void saveFabula(
			File filename, 
			String language) {

		try {

			FileOutputStream out = new FileOutputStream(filename);
			
			//TriXWriter writer = new TriXWriter();
			//writer.write(fabula, System.out, null); // workaround for nullpointerException in TriGWriter.addNamespace
			
/*			for (Map.Entry e : Config.namespaceMap.entrySet()) {
				logger
						.fine("Adding namespace\n" + e.getKey() + "\n"
								+ e.getValue());
				writer.addNamespace(e.getKey().toString(), e.getValue().toString());
			}
*/			
			//fabula.write(out, language, null);
			//writer.write(fabula, out, null);
			fabula.write(out, language, null);

			out.close();
			
		} catch (FileNotFoundException e) {
			logger.severe(e.getLocalizedMessage());
		} catch (IOException e) {
			e.printStackTrace();
		}		
	
	}	

}
