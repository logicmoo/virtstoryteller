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
package vs.worldagent;

import vs.communication.Operator;

/**
 * Represents a data structure for the timing of an action :
 * 
 * |                           duration                     |
 * |  interruptableDuration  |
 * |-------------------------|------------------------------|
 * startTime                 interTime                      endTime
 * 
 * @author swartjes
 * Created on 15-jul-2005
 */
public class ScheduledOperator {
	public static final int CREATED = 1;
	public static final int SCHEDULED = 2;
	public static final int STARTED = 3;
	public static final int NO_LONGER_INTERRUPTABLE = 4;
	public static final int FINISHED = 5;
	
	public static final int ABORTED = 6;
	private Operator m_operator;
	private int m_startTime;
	private int m_interTime;
	private int m_endTime;
	private int m_status;
	
	/**
	 * Constructor
	 * 
	 * @param startTime The starting time of this schedule
	 * @param interTime The time when the intereffects are applied
	 * @param endTime The time when this schedule ends and the effects are applied
	 */
	public ScheduledOperator(Operator operator, int startTime, int interTime, int endTime) {
		m_operator = operator;
		m_startTime = startTime;
		m_interTime = interTime;
		m_endTime = endTime;
		m_status = ScheduledOperator.CREATED;
	}
	
	/**
	 * @return end time
	 */		
	public int getEndTime() {
		return m_endTime;
	}
	
	/**
	 * @return intereffects time
	 */	
	public int getInterTime() {
		return m_interTime;
	}

	/**
	 * @return the action
	 */
	public Operator getOperator() {
		return m_operator;
	}

	/**
	 * @return start time
	 */
	public int getStartTime() {
		return m_startTime;
	}
	
	/**
	 * Get status of this scheduled action
	 * @return the status of this scheduled action
	 */
	public int getStatus() {
		return m_status;
	}
	
	/**
	 * Set status of this scheduled action
	 * @param newStatus the new status of this scheduled action
	 */
	public void setStatus(int newStatus) {
		m_status = newStatus;
	}
	
	/** 
	 * String representation of scheduled action
	 */
	@Override
	public String toString() {
		StringBuffer buf = new StringBuffer();
		buf.append("[");
		buf.append("START: t=").append(m_startTime).append(" ");
		buf.append("INTER: t=").append(m_interTime).append(" ");
		buf.append("END: t=").append(m_endTime).append(" ");
		buf.append("Status: ").append(m_status).append("]\n");
		
		buf.append(m_operator);
		
		return buf.toString();
	}

}
