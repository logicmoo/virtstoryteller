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
import java.util.Set;

import vs.IAgentModule;
import vs.communication.FabulaCausality;
import vs.communication.FabulaElement;

/**
 * Responsible for building a fabula structure from everything
 * that happens in the Story World.
 * 
 * @author swartjes
 */
public interface IFabulaBuilder extends IAgentModule {
	
	public void addFabulaCausality(FabulaCausality fc);
	
	public void addFabulaElement(FabulaElement fe);

	public Set<FabulaKnowledgeBase> getAllFabulaKnowledgeBase();
	
	public void registerFabulaKnowledgeBase(FabulaKnowledgeBase storage);
	
	
	/**
	 * Saves the fabula to a file using a certain language
	 * 
	 * @param filename the filename of the fabula file
	 * @param language the language (see Jena)
	 */
	public void saveFabula(File filename, String language);
	
	
	
	
}
