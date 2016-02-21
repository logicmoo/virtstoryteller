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
package vs.utils;

/**
 * Stores a long to be used as a counter for unique numbers 
 * @author KruizingaEE and swartjes
 * 10-5-2007
 */


public class UniqueId extends Object {
	
	private static int _uniqueCounter = 0;
	private static final char separator = '_';
	
	public static String generateUniqueAgentName(String type, String ID) {
		UniqueId._uniqueCounter++;
		StringBuilder uniqueName = new StringBuilder();
		uniqueName.append(type).append(UniqueId.separator)
					.append(ID).append(UniqueId.separator).append(String.valueOf(UniqueId._uniqueCounter));
		
		return uniqueName.toString().replace(':', '_');
	}	
	
	/**
	 * Return a new indiviual name with a unique counter number
	 * @return a String representing the unique individual name
	 */
	public static String generateUniqueIndividual(String type, String ID) {
		UniqueId._uniqueCounter++;
		StringBuilder uniqueName = new StringBuilder();
		uniqueName.append("#i")//.append(UniqueId.separator)
					.append(type).append(UniqueId.separator)
					.append(ID).append(UniqueId.separator).append(String.valueOf(UniqueId._uniqueCounter));
		
		return uniqueName.toString().replace(':', '_');

	}
	
}
