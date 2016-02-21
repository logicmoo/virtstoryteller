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
import java.util.logging.LogRecord;

/**
 * This custom formatter formats parts of a log record.
 * @author swartjes
 * Created on 7-jun-2005
 */
public class NiceLogFormatter extends Formatter {

    // This method is called for every log records
    @Override
	public String format(LogRecord rec) {
        StringBuffer buf = new StringBuffer(1000);

        buf.append(rec.getLevel());
        for (int x = 0; x < 8 - (rec.getLevel().toString().length()); x++) {
        	buf.append(' ');
        }

        buf.append("| ");
        buf.append(formatMessage(rec));
        buf.append(" (@ ");
        buf.append(rec.getSourceClassName());
        buf.append(".");        
        buf.append(rec.getSourceMethodName());
        buf.append("()) ");
        buf.append('\n');
        return buf.toString();
    }

    // This method is called just after the handler using this
    // formatter is created
    @Override
	public String getHead(Handler h) {
        return "Log date: " + (new Date()) + "\n------------------------------------\n";
    }

    // This method is called just after the handler using this
    // formatter is closed
    @Override
	public String getTail(Handler h) {
        return "------------------------------------\n";
    }

}
