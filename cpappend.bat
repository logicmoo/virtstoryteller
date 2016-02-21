if "%libjars%" == "" goto setInitial
set libjars=%libjars%;%1
goto finish

:setInitial
set libjars=%1

:finish
