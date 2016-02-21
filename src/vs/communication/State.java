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
* Protege name: State
* @author ontology bean generator
* @version 2008/10/29, 15:01:49
*/
public class State extends FabulaElement{ 

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

   /**
* Protege name: contentFabula
   */
   private List contentFabula = new ArrayList();
   public void addContentFabula(Object elem) { 
     List oldList = this.contentFabula;
     contentFabula.add(elem);
   }
   public boolean removeContentFabula(Object elem) {
     List oldList = this.contentFabula;
     boolean result = contentFabula.remove(elem);
     return result;
   }
   public void clearAllContentFabula() {
     List oldList = this.contentFabula;
     contentFabula.clear();
   }
   public Iterator getAllContentFabula() {return contentFabula.iterator(); }
   public List getContentFabula() {return contentFabula; }
   public void setContentFabula(List l) {contentFabula = l; }

}
