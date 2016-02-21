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

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;

/**
 * Contains configuration settings.
 * TODO: Move this to a text file?
 * 
 * @author swartjes
 * Created on 29-jun-2005
 */
public class Config {
	
	/** The debug level of the log factory **/
	public static Level DEBUGLEVEL = Level.FINE;
	
	/** The relative path of the log files **/
	public static String LOGPATH = "log/";	
	public static String KNOWLEDGEPATH = "knowledge/";
	public static String ONTOLOGYPATH = Config.KNOWLEDGEPATH + "ontology/";
	public static String PROLOGFILESPATH = Config.KNOWLEDGEPATH + "prolog/";
	public static String DOMAINSPATH = Config.KNOWLEDGEPATH + "domain/";
	public static String CHARACTERPROLOGFILESPATH = Config.PROLOGFILESPATH + "CharacterAgent/";
	public static String PLOTPROLOGFILESPATH = Config.PROLOGFILESPATH + "PlotAgent/";
	public static String PROPERTIESFILE = "conf/vst.properties";
	public static String PROPERTIES_STATEFILE = "conf/vst_state.properties";
	
	public static String RUN_PROCESS_BATCHFILE = "process_launcher.bat";
	
	//public static String INSPIRATIONRULESPATH = Config.DOMAINPATH + "cases/";
	
	/** Main ontology locations */
	public static String SWC_URI = Config.ONTOLOGYPATH + "StoryWorldCore.owl"; // core ontology
	public static String FABULA_URI = Config.ONTOLOGYPATH + "FabulaKnowledge.owl";
	
	/** Namespace Mappings **/	
	public static String SWC_PREFIX = "swc";
	public static String FABULA_PREFIX = "fabula";

	public static Map<String,String> namespaceMap = new HashMap<String,String>();
	static{
		// create namespace mappings for OWL, RDF and RDFS
		Config.namespaceMap.put("owl", "http://www.w3.org/2002/07/owl#");
		Config.namespaceMap.put("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#");
		Config.namespaceMap.put("rdfs", "http://www.w3.org/2000/01/rdf-schema#");
		Config.namespaceMap.put("xsd", "http://www.w3.org/2001/XMLSchema#");
		
		// create extra namespace mappings for GSW, SSW and STORY
		Config.namespaceMap.put(Config.SWC_PREFIX, "http://www.owl-ontologies.com/StoryWorldCore.owl#");
		Config.namespaceMap.put(Config.FABULA_PREFIX, "http://www.owl-ontologies.com/FabulaKnowledge.owl#");
		Config.namespaceMap.put("cind", "http://www.owl-ontologies.com/StoryWorldSettings/Cinderella#");
		Config.namespaceMap.put("ps", "http://www.owl-ontologies.com/StoryWorldSettings/Pirates#");
		Config.namespaceMap.put("lolli", "http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#");
		Config.namespaceMap.put("love", "http://www.owl-ontologies.com/StoryWorldSettings/MakeLoveNotWar.owl#");
		Config.namespaceMap.put("red", "http://www.owl-ontologies.com/Red.owl#");
		Config.namespaceMap.put("graph", "http://www.owl-ontologies.com/Graphs.owl#");
	}
	
}

