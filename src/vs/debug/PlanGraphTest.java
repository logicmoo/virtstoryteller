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

import javax.swing.JFrame;

import vs.characteragent.ui.PlanGraphPanel;

public class PlanGraphTest {

	public static void main(String args[]) {
		
		JFrame newFrame = new JFrame();
		PlanGraphPanel pgp = new PlanGraphPanel(new DummyPlanner());
		newFrame.setContentPane(pgp);
		
		newFrame.pack();
		newFrame.setVisible(true);
		pgp.refresh();		
		newFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		//assertTrue(true);
	}
	
}
