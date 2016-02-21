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
* Protege name: GoalProgress
* @author ontology bean generator
* @version 2008/10/29, 15:01:49
*/
public class GoalProgress implements Predicate {

//////////////////////////// User code
public static final int COMPLETED = 0;
public static final int FAILED = 1;
public static final int WAITFOREXPECTATION = 2;
public static final int NEARLYCOMPLETED = 3;
public static final int UNKNOWN = 4;
   /**
   * The goal the character was performing
* Protege name: goal
   */
   private String goal;
   public void setGoal(String value) { 
    this.goal=value;
   }
   public String getGoal() {
     return this.goal;
   }

   /**
   * The status of this goal
* Protege name: goalstatus
   */
   private int goalstatus;
   public void setGoalstatus(int value) { 
    this.goalstatus=value;
   }
   public int getGoalstatus() {
     return this.goalstatus;
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

}
