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

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Logger;

import vs.Config;

/**
 * Sets up and manages loggers
 * 
 * @author swartjes
 * Created on 29-jun-2005
 */
public class LogFactory {
	
	private static List<String> loggers = new ArrayList<String>();
	public static long timeOffset = 0;
	public static String m_agentName = null;
	
	/** 
	 * Closes all handlers of the logger for this package
	 * 
	 * @param loggerName the name of the logger
	 */
	public static void closeLogger(String loggerName) {
		String pkg = LogFactory.makeLoggerName(loggerName);
		if (LogFactory.loggers.contains(pkg)) {
			Logger l = Logger.getLogger(pkg);
			Handler[] hlist = l.getHandlers();

			for (Handler element : hlist) {
				System.out.println("Handler " + element);
				element.close();
			}
		}
	}
	
	/**
	 * Generates index HTML file containing all the log files as links
	 *
	 */
	public static void generateIndex() {
		
		// Generate general index
		
		// Generate agent-specific index
		try {
			FileWriter out = new FileWriter(getIndex());
			PrintWriter p = new PrintWriter(out);
			
			for (int i = 0; i < LogFactory.loggers.size(); i++) {
				String currLogger = LogFactory.loggers.get(i);
				StringBuffer b = new StringBuffer(100);
				b.append("<A HREF=\"./").append(currLogger).append(".html\">"); 
				b.append(LogFactory.loggers.get(i));
				b.append("</A><BR>");
				p.print(b.toString());
			}
			
			out.close();
			
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	}
	
	public static String getIndex() {
		return Config.LOGPATH + "_index_" + m_agentName + ".html";
	}
	
	/**
	 * Returns a logger for the package of the object; sets it up if not used before
	 * @param src the Object that is getting the logger
	 * @return the Logger
	 */
	public static Logger getLogger(Object src) {
		String pkg = LogFactory.makeLoggerName(src.getClass().getName());
		if (LogFactory.loggers.contains(pkg)) {
			return Logger.getLogger(pkg);
		}
		else {
			Logger newLogger = LogFactory.setupLogger(pkg);
			// There is a new logger. Adjust index file.
			LogFactory.generateIndex();
			return newLogger;
		}		
	}
	
	/**
	 * @deprecated 
	 * Use getLogger(Object src) instead.
	 * 
	 * Returns the logger for this package; sets it up if not used before
	 * 
	 * @param loggerName the name of the logger
	 * @return the Logger
	 */
	@Deprecated
	public static Logger getLogger(String loggerName) {
		String pkg = LogFactory.makeLoggerName(loggerName);
		if (LogFactory.loggers.contains(pkg)) {
			return Logger.getLogger(pkg);
		}
		else {
			Logger newLogger = LogFactory.setupLogger(pkg);
			// There is a new logger. Adjust index file.
			LogFactory.generateIndex();
			return newLogger;
		}
	}
	
	/*public static Logger getLogger(String AgentName, String loggerName){
		return getLogger(AgentName + "." + loggerName);
	}*/

	private static String makeLoggerName(String pkg) {
		if (LogFactory.m_agentName != null) {
			return LogFactory.m_agentName + "." + pkg;
		} else {
			return pkg;
		}
	}
	
	/**
	 * Registers agent name to this LogFactory, to prepend to package name
	 * 
	 * @param agentName description of agent.
	 */
	public static void registerAgent(String agentName) {
		LogFactory.m_agentName = agentName;
		
		// Make log file for whole agent, so that everything that
		// is logged will propagate to a single mother file as well
		LogFactory.getLogger("vs");
	}
	
	/** 
	 * Creates a new HTML logger
	 * 
	 * @param pkg the package name of the logger
	 * @return the Logger
	 */
	private static Logger setupLogger(String pkg) {
		// Set timer
		if (LogFactory.timeOffset == 0) {
			Date d = new Date();
			LogFactory.timeOffset = d.getTime();
		}
		// Make logger
		Logger logger = Logger.getLogger(pkg);
		logger.setLevel(Config.DEBUGLEVEL);

		try {
			FileHandler handler = new FileHandler(Config.LOGPATH + pkg + ".html");
			HTMLLogFormatter formatter = new HTMLLogFormatter();
			formatter.setTitle(pkg);
			
			handler.setFormatter(formatter);
			logger.addHandler(handler);	
		} 
		catch (IOException e) {
			e.printStackTrace();
		}
		
		logger.config("Starting log");		
		LogFactory.loggers.add(pkg);
		return logger;
	}

}
