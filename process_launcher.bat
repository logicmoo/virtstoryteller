@rem -c
@echo off
:setjava
set javabase=.
set srcdir=%javabase%\src
set classdir=%javabase%\bin
set libdir=%javabase%\lib
for %%i in (%libdir%\jade-3.4\*.jar) do call cpappend.bat %%i
for %%i in (%libdir%\ng4j-0.8\*.jar) do call cpappend.bat %%i
for %%i in (%libdir%\JUNG\*.jar) do call cpappend.bat %%i
for %%i in (%libdir%\*.jar) do call cpappend.bat %%i
set datapath=;%javabase%\data
set classpath=%classdir%%datapath%%libjars%
set package=
set docsdir=%javabase%\doc\%package%
set otherjavacflags=
set otherjavaflags=
set otherjavadocflags=
:endsetjava

REM Set prolog environment

path = "C:\Program Files\Java\jdk1.6.0_04\bin";%path%;%javabase%\..\swi-prolog-jpl\pl\bin

REM Find the Prolog coordinates
plcon.exe -dump-runtime-variables=cmd > %TEMP%\plrtvars.bat

call %TEMP%\plrtvars.bat

del %TEMP%\plrtvars.bat

REM Find classpath for jpl.jar.  First case holds if we are in the source tree.

:java
set classpath=%classpath%;%javabase%\..\swi-prolog-jpl\pl\lib\jpl.jar;.
@echo on
java -classpath %classpath% %*
@echo off
:exit
