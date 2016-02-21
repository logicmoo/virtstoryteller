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
package vs.debug;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Hashtable;
import java.util.Properties;

import jpl.Query;
import vs.knowledge.PrologKB;

public class JPrologTest {

	
	/**
	 * @param args
	 */
	public static void main(String[] args) {

		try {
			//setPrologEnv();
		} catch(Exception e) {
			e.printStackTrace();
		}
		
		PrologKB km = 
			PrologKB.getInstance();
		
		String term1 = "(owl:'Thing', rdf:type, owl:'Class')";
		String term2 = "('boom', 'roos', 'vis')";
		if (km.tellRDF(term1)) {
			System.out.println(term1 + " told (asserted).");
		}
		if (km.tellRDF(term2)) {
			System.out.println(term2 + " told (asserted).");
		}
		
		String query1 = "(S, P, O)";
		Query q = km.query(query1);
		Hashtable[] solutions = q.allSolutions();
		System.out.println("Query " + query1 + " gives these solutions: ");	
		for (Hashtable element : solutions) {
			System.out.println(element.toString());
		}		

		
		if (km.untellRDF(term1)) {
			System.out.println(term1 + " untold.");
		}
		if (km.untellRDF(term2)) {
			System.out.println(term2 + " untold.");
		}

		String query2 = "(S, rdf:type, O)";
		Query q2 = km.query(query2);
		Hashtable[] solutions2 = q2.allSolutions();
		System.out.println("it gives these solutions: ");	
		for (Hashtable element : solutions2) {
			System.out.println(element.toString());
		}
	}
	
/*	private static void runProlog() {

		Runtime runtime = Runtime.getRuntime();
		String[] cmd =
		{"cmd","/c","\\local\\Edze\\prolog\\pl\\doc\\packages\\examples\\jpl\\java\\env.bat"};
		try{

			setPrologEnv();
			//Process p =runtime.exec(cmd);
//
//		BufferedReader b =new BufferedReader(new
//		InputStreamReader(p.getInputStream()));
//		String line=null;
//		while( (line=b.readLine())!=null)
//		{
//
//		System.out.println(line);
//
//		}

		 }
		catch(Exception e)
		{

		e.printStackTrace();
		}

	}*/
	
	
	private static void setPrologEnv() throws Exception {
		Process p = null;
		Properties envVars = new Properties();
		String command = "";
		
		// Run Prolog's env.bat
		Runtime r = Runtime.getRuntime();
		String OS = System.getProperty("os.name").toLowerCase();
		if (OS.indexOf("windows 9") > -1) {
			command = "command.com /c";
			p = r.exec( "command.com /c" );
	    }
	  else if ( (OS.indexOf("nt") > -1)
			  || (OS.indexOf("windows 2000") > -1 )
			  || (OS.indexOf("windows xp") > -1) ) {

			command = "cmd.exe /c";		  
	    }
	  else {
	    // our last hope, we assume Unix (thanks to H. Ware for the fix)
			command = "";
	    }

		command += " C:\\local\\Edze\\prolog\\pl\\bin\\plcon -dump-runtime-variables=cmd";
		p = r.exec(command);

		BufferedReader br = new BufferedReader ( new InputStreamReader( p.getInputStream() ) );
		String line;
		while( (line = br.readLine()) != null ) {
			
			// Strip "SET " part
			int set_idx = line.indexOf("SET ");
			if (set_idx > -1) {
				String stripline = line.substring(set_idx + 4);
				line = stripline;
			}
			int idx = line.indexOf( '=' );
			String key = line.substring( 0, idx );
			String value = line.substring( idx+1 );
			envVars.setProperty( key, value );
			System.out.println( key + " = " + value );
		}
		
		//System.getenv().putAll((java.util.Map)envVars);
		System.getenv().put("IK", "IVO");
		
		//System.getProperties().putAll(envVars);
		System.out.println("The environment variables are now: ");
		System.out.println(System.getenv());
		
	}

}
