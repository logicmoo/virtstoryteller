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
   * Commit oneself to play given part in the story, executed by Character Agent
* Protege name: PlayCharacter
* @author ontology bean generator
* @version 2008/10/29, 15:01:49
*/
public class PlayCharacter implements AgentAction {

   /**
   * the character info for the request
("do you want to join the story as [characterinfo]"?)
* Protege name: characterInfo
   */
   private CharacterInfo characterInfo;
   public void setCharacterInfo(CharacterInfo value) { 
    this.characterInfo=value;
   }
   public CharacterInfo getCharacterInfo() {
     return this.characterInfo;
   }

}
