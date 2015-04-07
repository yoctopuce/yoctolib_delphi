@ECHO OFF
REM --
REM -- BUILD SCRIPT FOR DELPHI XE2 PRO 
REM --
IF "%1" == "" goto build
IF %1 == clean goto clean
:build
SET SAVEPATH=%PATH%
REM -- Assume Delphi is in path: there is a script there that sets BDS variables
CALL RSVARS.BAT
SET PATH=%SAVEPATH%
SET SAVEPATH=
SET Platform=win32
SET BDS_COLL=Generics.Collections=System.Generics.Collections;Generics.Defaults=System.Generics.Defaults;WinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE
SET BDS_NS=Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Data;Datasnap;Web;Soap;Winapi; 
SET BDS_LIBPATH="%BDS%\lib\%Platform%\release";"%BDS%\Imports";"%BDS%\include";"%BDSCOMMONDIR%\Dcp";"%BDS%\include";"..\..\Sources"
SET BDS_PATHOPTS=-A%BDS_COLL% -I%BDS_LIBPATH% -LE"%BDSCOMMONDIR%\Bpl" -LN"%BDSCOMMONDIR%\Dcp" -NS%BDS_NS% -O%BDS_LIBPATH% -R%BDS_LIBPATH% -U%BDS_LIBPATH% -NB"%BDS%\Dcp" -NH"%BDSCOMMONDIR%\hpp"
SET BDS_STDOPTS=-$O- -$W+ --no-config -B -Q -DDEBUG -K00400000
dcc32.exe %BDS_STDOPTS% %BDS_PATHOPTS% YoctoTerminalDemo.dpr
IF ERRORLEVEL 1 goto error
if exist *.dcu del *.dcu /q /f
IF "%1" == "" goto continue
IF %1 == noexe goto clean
:continue
copy ..\..\Sources\dll\yapi.dll .
goto end

:clean
if exist *.dcu del *.dcu /q /f
if exist *.exe del *.exe /q /f
if exist yapi.dll del yapi.dll /q /f
goto end

:error
echo error
exit /b 1

:end