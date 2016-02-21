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
package vs;

public class StoryDomain {
	private String _name;
	private String _author;
	private String _description;
	private String _ontology;
	private String _altEntry;
	
	public StoryDomain(String name, String author, String description, String ontology, String altEntry) {
		_name = name;
		_author = author;
		_description = description;
		_ontology = ontology;
		_altEntry = altEntry;
	}
	
	public String getAltEntry() {
		return _altEntry;
	}
	
	public String getAuthor() {
		return _author;
	}
	
	public String getDescription() {
		return _description;
	}
	
	public String getName() {
		return _name;
	}
	
	public String getOntology() {
		return _ontology;
	}	
	
	@Override
	public String toString() {
		return "<B>Name: </B><font face=\"Courier New\">" + _name + "</font><BR><B>Description:</B> " + _description + "<BR><B>Author:</B> " + _author;
	}
}