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
   * Any element that occurs in fabula (e.g. goals, actions, events)
* Protege name: FabulaElement
* @author ontology bean generator
* @version 2008/10/29, 15:01:49
*/
public class FabulaElement implements Concept {

   /**
   * the Individual that the character represents in the world
* Protege name: individual
   */
   private String individual;
   public void setIndividual(String value) { 
    this.individual=value;
   }
   public String getIndividual() {
     return this.individual;
   }

   /**
* Protege name: type
   */
   private String type;
   public void setType(String value) { 
    this.type=value;
   }
   public String getType() {
     return this.type;
   }

   /**
   * refers to the entity in the story world that 'has' or 'intends' the fabula element.
* Protege name: character
   */
   private String character;
   public void setCharacter(String value) { 
    this.character=value;
   }
   public String getCharacter() {
     return this.character;
   }

   /**
   * time at which the fabula element was created
* Protege name: time
   */
   private int time;
   public void setTime(int value) { 
    this.time=value;
   }
   public int getTime() {
     return this.time;
   }

}
