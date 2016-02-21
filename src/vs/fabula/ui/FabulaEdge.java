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
package vs.fabula.ui;

import java.util.HashSet;
import java.util.Set;

public class FabulaEdge  {
    private static Set<FabulaEdge> _created = new HashSet<FabulaEdge>();
    public static FabulaEdge getEdge( String name, Object src, Object tgt) {
    	for (FabulaEdge fe: _created) {
    		if (fe.getName().equals(name) && fe.getSource().equals(src) && fe.getTarget().equals(tgt)) {
    			return fe;
    		}
    	}
    	   	
		FabulaEdge newEdge = new FabulaEdge(name, src, tgt);
		_created.add(newEdge);
		return newEdge;
    }
    private String name;
    
    private Object src;
    
    private Object tgt;

    protected FabulaEdge(String name, Object src, Object tgt) {
        this.name = name;	       
        this.src = src;
        this.tgt = tgt;    	
    }		
    
    public String getName() {
    	return name;
    }
    
    public Object getSource() {
    	return src;
    }	    
    
    public Object getTarget() {
    	return tgt;
    }
    
    @Override
	public String toString() {
    	return name;
    }
}	
	

