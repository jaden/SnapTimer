@echo off
REM Copy files to be deployed into the Deploy directory

set deploy=.\Deploy

copy /Y snaptimer.exe %deploy%\SnapTimer.exe
copy /Y Readme.txt %deploy%\Readme.txt
xcopy /Y /S sounds %deploy%\sounds
REM copy /Y snaptimer.chm %deploy%\SnapTimer.chm