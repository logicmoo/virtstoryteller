-----------------------
The Virtual Storyteller
-----------------------
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

------------
INTRODUCTION
------------
A story generator based on the concept of emergent narrative, i.e.,
- Stories produced by autonomous characters
- No target plots

------------
INSTALLATION
------------
The Virtual Storyteller currently only runs on MS Windows machines (tested on Windows XP).

(1) Install the following programs:
    - SWI-Prolog (5.6.37 or higher) from http://www.swi-prolog.org

(2) Copy the whole SWI-Prolog directory (by default: C:\Program Files\pl) so that the following directory structure occurs:
    \[MyDir]
        \VirtualStoryteller
            \bin
            \src
            \lib
            ...
        \SWI-Prolog-JPL
            \pl

-------     
RUNNING
-------
Running the class vs.AgentLauncher yields the platform from which to operate the Virtual Storyteller.
Executing run.bat will launch this class as a Java application.

Before you start ANY agent, first start the JADE platform. You can do both with a button in the GUI.

---------------
RECOMMENDATIONS
---------------
For working with the Virtual Storyteller, we recommend installing the latest version of the Protégé ontology editor
(http://protege.stanford.edu/) which you can use to edit the ontology files included in the Virtual Storyteller.
Furthermore, we recommend working with Eclipse using the Prolog plugin ProDT (http://prodevtools.sourceforge.net/) if
you want to make modifications to the source or author new story domains.