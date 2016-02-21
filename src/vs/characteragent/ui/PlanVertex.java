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
package vs.characteragent.ui;

public class PlanVertex {
    private String vertexID;
    private String label;
    private String type;

    public PlanVertex( String ID, String type, String label) {
       this.vertexID = ID;
       this.type = type;
       this.label = label;
    }	
    
    public String getType() {
    	return type;
    }
    
    @Override
	public String toString() {
    	if (label != null && type != null) {
    		com.hp.hpl.jena.shared.PrefixMapping pm = com.hp.hpl.jena.shared.PrefixMapping.Factory.create();
    		pm.setNsPrefixes(vs.Config.namespaceMap);
    		return type + ": " + pm.shortForm(label);
    	}
    	return vertexID;
    }
    
/*	*//**
	 * Following the steps from http://java.sun.com/developer/Books/effectivejava/Chapter3.pdf
	 * It must be symmetric, transitive and consistent
	 *//*
	@Override
	public boolean equals(Object o) {
		// 1. check for equality
		if (this == o)
			return true;

		// 2. check for type
		if (!(o instanceof PlanVertex))
			return false;

		// 3. typecast
		PlanVertex nv = (PlanVertex) o;

		// 4. Check for equality of important fields. 
		return nv.vertexID.equals(this.vertexID);
	}

	*//** 
	 * Return the sum of the hashcodes of the important parts. hashCode must be overridden when equals() is used.
	 *//*
	@Override
	public int hashCode() {
		int result = 17; // arbitrary
		result = 37 * result + vertexID.hashCode(); // 37 is an odd prime

		return result;
	}	*/    
    
}
