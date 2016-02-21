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
package vs.rationalagent.ui;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

public abstract class UpdateablePanel extends JPanel implements ActionListener {
	
	protected JButton refreshButton;
	protected JPanel buttonPanel;
	protected Component m_content;
	protected String m_name;
	protected JLabel nameLabel;
	
	public UpdateablePanel() {
		super(new BorderLayout());
		refreshButton = new JButton("Update");
		refreshButton.addActionListener(this);
		
		buttonPanel = new JPanel(new BorderLayout());
		buttonPanel.add(refreshButton, BorderLayout.WEST);
		
		add(buttonPanel, BorderLayout.SOUTH);
	}
	
	public void actionPerformed(ActionEvent e) {		
		if (e.getSource() == refreshButton) {
			this.setVisible(false);
			update();
			this.setVisible(true);
		}
	}
	
	public void setContent(Component c) {
		if (m_content != null) {
			remove(m_content);
		}
		m_content = c;
		add(m_content);
	}
	
	public abstract void update();
	
}
