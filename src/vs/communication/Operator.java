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
   * Something that can change the world state
* Protege name: Operator
* @author ontology bean generator
* @version 2008/10/29, 15:01:49
*/
public class Operator extends FabulaElement{ 

   /**
   * whether the Operator was successful (set by Plot Agent when communicating perceptions)
* Protege name: isSuccessful
   */
   private boolean isSuccessful;
   public void setIsSuccessful(boolean value) { 
    this.isSuccessful=value;
   }
   public boolean getIsSuccessful() {
     return this.isSuccessful;
   }

   /**
   * Agens of the operator
* Protege name: agens
   */
   private String agens;
   public void setAgens(String value) { 
    this.agens=value;
   }
   public String getAgens() {
     return this.agens;
   }

   /**
* Protege name: starttime
   */
   private int starttime;
   public void setStarttime(int value) { 
    this.starttime=value;
   }
   public int getStarttime() {
     return this.starttime;
   }

   /**
   * Target of the operator
* Protege name: target
   */
   private String target;
   public void setTarget(String value) { 
    this.target=value;
   }
   public String getTarget() {
     return this.target;
   }

   /**
   * Instrument of the operator
* Protege name: instrument
   */
   private String instrument;
   public void setInstrument(String value) { 
    this.instrument=value;
   }
   public String getInstrument() {
     return this.instrument;
   }

   /**
   * Prolog description of the operator
* Protege name: prologDescription
   */
   private String prologDescription;
   public void setPrologDescription(String value) { 
    this.prologDescription=value;
   }
   public String getPrologDescription() {
     return this.prologDescription;
   }

   /**
* Protege name: endtime
   */
   private int endtime;
   public void setEndtime(int value) { 
    this.endtime=value;
   }
   public int getEndtime() {
     return this.endtime;
   }

   /**
   * Patiens of the operator
* Protege name: patiens
   */
   private String patiens;
   public void setPatiens(String value) { 
    this.patiens=value;
   }
   public String getPatiens() {
     return this.patiens;
   }

}
