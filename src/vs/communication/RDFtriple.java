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
package vs.communication;


import jade.content.*;
import jade.util.leap.*;
import jade.core.*;

/**
   * Represents a fact in the form of an RDF triple
(NOTE: contains code to pretty print, and to compare triples for equality)
* Protege name: RDFtriple
* @author ontology bean generator
* @version 2008/10/29, 15:01:49
*/
public class RDFtriple implements Concept {

//////////////////////////// User code
public String toString() {
		com.hp.hpl.jena.shared.PrefixMapping pm = com.hp.hpl.jena.shared.PrefixMapping.Factory.create();
		pm.setNsPrefixes(vs.Config.namespaceMap);

		StringBuilder sb = new StringBuilder();
		sb.append(getTruth()).append("(").append(pm.shortForm(getSubject())).append(",").append(
				pm.shortForm(getPredicate())).append(",").append(pm.shortForm(getObject())).append(
				")");
		return sb.toString();
}

/**
 * Following the steps from http://java.sun.com/developer/Books/effectivejava/Chapter3.pdf
 * It must be symmetric, transitive and consistent
 */
@Override
public boolean equals(Object o) {
	// 1. check for equality
	if (this == o)
		return true;

	// 2. check for type
	if (!(o instanceof RDFtriple))
		return false;

	// 3. typecast
	RDFtriple t = (RDFtriple) o;

	boolean equalCheck = true;

	// 4. Check for equality of important fields. 
	//    We are only interested in the antecedents, consequents, and inspiration-causers.
	return t.getTruth() == this.getTruth()
			&& t.getSubject().equals(this.getSubject())
			&& t.getPredicate().equals(this.getPredicate())			
			&& t.getObject().equals(this.getObject());
}

/** 
 * Return the sum of the hashcodes of the important parts. hashCode must be overridden when equals() is used.
 */
@Override
public int hashCode() {
	int result = 17; // arbitrary
	result = 37 * result + (getTruth()? 1 : 2); // 37 is an odd prime
	result = 37 * result + getSubject().hashCode();
	result = 37 * result + getPredicate().hashCode();
	result = 37 * result + getObject().hashCode();

	return result;
}
   /**
   * the object of a triple (a URI)
* Protege name: object
   */
   private String object;
   public void setObject(String value) { 
    this.object=value;
   }
   public String getObject() {
     return this.object;
   }

   /**
   * whether to add, or delete (true = add, false = delete)
* Protege name: truth
   */
   private boolean truth;
   public void setTruth(boolean value) { 
    this.truth=value;
   }
   public boolean getTruth() {
     return this.truth;
   }

   /**
   * the predicate of a triple (an URI)
* Protege name: predicate
   */
   private String predicate;
   public void setPredicate(String value) { 
    this.predicate=value;
   }
   public String getPredicate() {
     return this.predicate;
   }

   /**
   * the subject of a triple (an URI)
* Protege name: subject
   */
   private String subject;
   public void setSubject(String value) { 
    this.subject=value;
   }
   public String getSubject() {
     return this.subject;
   }

}
