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

import java.util.Date;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;

/**
 * This custom formatter formats parts of a log record.
 * @author swartjes
 * Created on 7-jun-2005
 */

public class HTMLLogFormatter extends Formatter {

	// {Foreground, background}
	public static String[] HEADER = {"#FFFFFF", "#000000"};
	public static String TABLEBG = "#F3F3F3";
	//public static String METHODCLASS = "#DDDDFF";
	//public static String FINEST = "#DDDDFF";
	//public static String FINER = "#BBBBFF";
	public static String[] FINE = {"#AAAAAA", HTMLLogFormatter.TABLEBG};
	//public static String CONFIG = "#AAAAFF";
	public static String[] INFO = {"#000000", HTMLLogFormatter.TABLEBG};
	public static String[] WARNING = {"#E56717", HTMLLogFormatter.TABLEBG};
	public static String[] SEVERE = {"#FF0000", HTMLLogFormatter.TABLEBG};
	public static String escapeHTML(String src) {
        StringBuffer out = new StringBuffer();
        for(int i=0; i<src.length(); i++)
        {
            char c = src.charAt(i);
            if(c > 127 || c=='"' || c=='<' || c=='>')
            {
               out.append("&#"+(int)c+";");
            }
            else
            {
                out.append(c);
            }
        }
        return out.toString();    	
    }

    private String m_title = "";
	//private long millis = 0; 
    
    /** 
     * Formats a log record
     * 
     * @param rec The record to format
     */
    @Override
	public String format(LogRecord rec) {
    	// Set time
    	double printMillis;
    	
/*    	if (millis == 0) {
    		millis = rec.getMillis();
    		printMillis = 0;
    	} else {*/
    		printMillis = ((rec.getMillis() - LogFactory.timeOffset))/1000.0; 
    	//}
    	
    	// Determine level and formatting
        StringBuffer buf = new StringBuffer(1000);
        String level = rec.getLevel().toString();
        Level l = rec.getLevel();
        String[] settings = HTMLLogFormatter.INFO;
        if (level.equals("SEVERE")) {
        	settings = HTMLLogFormatter.SEVERE;
        	level = "<B>" + level + "</B>";
        }
        if (level.equals("WARNING")) {
        	settings = HTMLLogFormatter.WARNING;
        }                
        if (l.intValue() <= Level.FINE.intValue()) {
        	settings = HTMLLogFormatter.FINE;
        }
        
        // Start
        buf.append("<TR>");       
        
        // Method + class
        buf.append(genCell("<small><B>" + level + "</B> (" + printMillis + "s)<BR>" + rec.getSourceMethodName() + "<BR>" + rec.getSourceClassName()+ "</small>", settings));
		
		// Level                  
		//buf.append(genCell("<small>" + level + "</small>", settings));			
        
		// Message
		buf.append(genCell("<pre>" + HTMLLogFormatter.escapeHTML(formatMessage(rec)) + "</pre>", settings));
        
        // End
		buf.append("</TR>");        
        buf.append('\n');
        return buf.toString();
    }
    
    /** 
     * Generates a table cell, with given settings
     * 
     * @param msg The HTML contents of the cell
     * @param settings The settings (like foreground color, background color)
     * @return the HTML of the cell
     */
    public String genCell(String msg, String[] settings) {
    	StringBuffer buf = new StringBuffer(1000);
    	buf.append("<TD bgcolor=").append(settings[1]).append(">\n");
    	buf.append("<font color=").append(settings[0]).append(">");
    	buf.append(msg);
    	buf.append("\n</font></TD>\n");
    	return buf.toString();
    }

    /**
     * Returns the head of the log file
     * 
     * @param h the handler
     */
    @Override
	public String getHead(Handler h) {
    	StringBuffer buf = new StringBuffer(100);
    	buf.append("<HTML><HEAD>");
		buf.append("<TITLE>").append(m_title).append("</TITLE>");
		buf.append("</HEAD><BODY>");
		buf.append("<H1>Log for: ").append(m_title).append("</H1>");
		buf.append("Created on: ").append(new Date()).append("<BR>");
    	buf.append("<TABLE border=0 width=100%>");
    	buf.append("<TR bgcolor=").append(HTMLLogFormatter.HEADER[1]).append(">");
    	buf.append(genCell("<B>Info</B>", HTMLLogFormatter.HEADER));    
    	//buf.append(genCell("<B>Level</B>", HEADER));
    	buf.append(genCell("<B>Message</B>", HTMLLogFormatter.HEADER));
    	buf.append("</TR>");
    	return buf.toString();
    }

    /**
     * Returns the tail of the log file
     * 
     * @param h the handler
     */
    @Override
	public String getTail(Handler h) {
    	StringBuffer buf = new StringBuffer(50);
    	buf.append("</TABLE></BODY></HTML>");
    	return buf.toString();
    }
    
    /** 
     * Sets the title of this log file, displayed on top.
     * 
     * @param title the title
     */
    public void setTitle(String title) {
    	m_title = title;
    }

}
