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
* Protege name: FabulaCausality
* @author ontology bean generator
* @version 2008/10/29, 15:01:49
*/
public class FabulaCausality implements Concept {

   /**
   * The property as string (i.e. http://...#psi_causes)
* Protege name: causalProperty
   */
   private String causalProperty;
   public void setCausalProperty(String value) { 
    this.causalProperty=value;
   }
   public String getCausalProperty() {
     return this.causalProperty;
   }

   /**
   * The subject of the link (a string representing the individual
* Protege name: subjectIndividual
   */
   private String subjectIndividual;
   public void setSubjectIndividual(String value) { 
    this.subjectIndividual=value;
   }
   public String getSubjectIndividual() {
     return this.subjectIndividual;
   }

   /**
   * the object of the link (as Individual)
* Protege name: objectIndividual
   */
   private String objectIndividual;
   public void setObjectIndividual(String value) { 
    this.objectIndividual=value;
   }
   public String getObjectIndividual() {
     return this.objectIndividual;
   }

}
