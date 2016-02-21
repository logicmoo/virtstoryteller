@rem -c
@echo off
:setjava
set javabase=.
set srcdir=%javabase%\src
set classdir=%javabase%\bin
set libdir=%javabase%\lib
set libjars=
for %%i in (%libdir%\*.jar) do call cpappend.bat %%i
set datapath=%javabase%\data
set classpath=%classdir%;%datapath%;%libjars%
set package=
set docsdir=%javabase%\docs\%package%
set otherjavacflags=
set otherjavaflags=
set otherjavadocflags=
:endsetjava
if "%1" == "-java" goto java
if "%1" == "-jr"   goto java
if "%1" == "-j"    goto java

javac -classpath %classpath% -sourcepath %srcdir% -d %classdir% %*
goto exit
:java
shift
set arglist= %1
:getarg
shift
if "%1" == "" goto run
set jfile=%1
set arglist=%arglist% %jfile%
goto getarg
:run
PATH = %PATH%;.\lib\
java -classpath %classpath% %arglist%
:exit
