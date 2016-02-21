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
   * WorldChange(x, y, z)
A change in the story world
* Protege name: WorldChange
* @author ontology bean generator
* @version 2008/10/29, 15:01:49
*/
public class WorldChange implements Predicate {

   /**
   * The thing (e.g. Action) that causes this world change. Empty could be interpreted as "no cause", i.e. a setting change.
* Protege name: cause
   */
   private String cause;
   public void setCause(String value) { 
    this.cause=value;
   }
   public String getCause() {
     return this.cause;
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
   * The contents of a fabula element, represented as a list of RDF triples
* Protege name: contentTriple
   */
   private List contentTriple = new ArrayList();
   public void addContentTriple(RDFtriple elem) { 
     List oldList = this.contentTriple;
     contentTriple.add(elem);
   }
   public boolean removeContentTriple(RDFtriple elem) {
     List oldList = this.contentTriple;
     boolean result = contentTriple.remove(elem);
     return result;
   }
   public void clearAllContentTriple() {
     List oldList = this.contentTriple;
     contentTriple.clear();
   }
   public Iterator getAllContentTriple() {return contentTriple.iterator(); }
   public List getContentTriple() {return contentTriple; }
   public void setContentTriple(List l) {contentTriple = l; }

}
