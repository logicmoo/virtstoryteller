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

import java.util.HashMap;
import java.util.Map;

import com.hp.hpl.jena.ontology.Individual;

public class FabulaVertex {
    public static enum FabulaKind {Event, Goal, Action, Outcome, Perception, Setting, InternalElement, FabulaElement}
    private static Map<Individual, FabulaVertex> _created = new HashMap<Individual, FabulaVertex>();
    private static Map<FabulaKind, String> typeDesc = new HashMap<FabulaKind, String>();
    static {
    	typeDesc.put(FabulaKind.Event, "E");
    	typeDesc.put(FabulaKind.Goal, "G");
    	typeDesc.put(FabulaKind.Action, "A");
    	typeDesc.put(FabulaKind.Outcome, "O");
    	typeDesc.put(FabulaKind.Perception, "P");
    	typeDesc.put(FabulaKind.Setting, "S");
    	typeDesc.put(FabulaKind.InternalElement, "IE");
    }
    
    public static FabulaVertex getVertex(Individual individual, String rdf_type, FabulaKind kind) {
    	FabulaVertex nwVertex = _created.get(individual);
    	if (nwVertex == null) {
    		nwVertex = new FabulaVertex(individual, rdf_type, kind);
    		_created.put(individual, nwVertex);
    	}
		return nwVertex;
    }
    private Individual individual;
    private FabulaKind kind;;

    private String rdf_type;	
    
    protected FabulaVertex( Individual individual, String rdf_type, FabulaKind kind) {
       this.individual = individual;
       this.rdf_type = rdf_type;
       this.kind = kind;
    }
    
    public Individual getIndividual() {
    	return individual;
    }
    
    public FabulaKind getKind() {
    	return kind;
    }
    
    public String getRDFType() {
    	return rdf_type;
    }
    
    @Override
	public String toString() {
    	if (kind != null && rdf_type != null) {
    		com.hp.hpl.jena.shared.PrefixMapping pm = com.hp.hpl.jena.shared.PrefixMapping.Factory.create();
    		pm.setNsPrefixes(vs.Config.namespaceMap);
    		StringBuilder sb = new StringBuilder();
    		sb.append("<HTML><div align=center><b>");
    		sb.append(typeDesc.get(getKind())).append("</b>");
    		if (! (getKind().equals(FabulaKind.Perception) 
    				|| getKind().equals(FabulaKind.InternalElement)
    				|| getKind().equals(FabulaKind.Setting))) {
    			sb.append("<BR>").append(pm.shortForm(rdf_type));
    		}
    		sb.append("</div></HTML>");
    		
    		return sb.toString();
    		
    	}
    	return individual.getLocalName();
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
