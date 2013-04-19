@ECHO OFF
echo.

echo Build Examples for Delphi
echo =========================

set failled=
FOR /D %%A IN (Examples\*) DO (call:BuildDir %%A %1)
IF NOT DEFINED failled goto ExamplesOK

echo.
echo COMPILATION HAS FAILLED ON DIRECTORIES :
echo %failled%

goto error
REM: ---  pseudo function to build all examples
:BuildDir
echo build %~1 %~2
cd %~1
call build.bat %~2
IF ERRORLEVEL 1 set failled=%failled% %~1
cd ..\..\
echo done
GOTO:EOF
goto end


:ExamplesOK
IF "%1" == "" goto end
echo clean dcu files
del  Sources\*.dcu 

goto end

:error
echo error
exit /b 1
:end





