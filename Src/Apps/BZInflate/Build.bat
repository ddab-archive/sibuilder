@rem ---------------------------------------------------------------------------
@rem Script used to build DelphiDabbler SITools BZInflate library.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2008
@rem
@rem v1.0 of 09 Feb 2008 - Original version.
@rem ---------------------------------------------------------------------------

@echo off
setlocal

set SrcDir=.\
set BinDir=..\..\..\Bin\BZInflate\
set DevTools=..\..\..\DevTools\
set Project=BZInflate

set BuildResources=
set BuildPascal=

if "%~1" == "" goto Config_BuildAll
if "%~1" == "all" goto Config_BuildAll
if "%~1" == "res" goto Config_BuildResources
if "%~1" == "pas" goto Config_BuildPascal

:Config_BuildAll
set BuildResources=1
set BuildPascal=1
goto Config_End

:Config_BuildResources
set BuildResources=1
goto Config_End

:Config_BuildPascal
set BuildPascal=1
goto Config_End

:Config_End
goto Build_Resources


rem ----------------------------------------------------------------------------
rem Build resources
rem ----------------------------------------------------------------------------

:Build_Resources
if not defined BuildResources goto Build_Pascal

echo.
echo BZInflate: Building Resources
echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
echo.

rem Version info resource: VBZInflate.vi
set VIBase=VBZInflate
set VISrc="%SrcDir%%VIBase%.vi"
set VITmp="%SrcDir%%VIBase%.rc"
set VIRes="%BinDir%%VIBase%.res"

set ErrorMsg=
echo Compiling %VIBase%.vi

%VIEd% -makerc %VISrc%
if errorlevel 1 set ErrorMsg=Failed to compile %VISrc%
if not "%ErrorMsg%"=="" goto VerInfoRes_End
%BRCC% %VITmp% -fo%VIRes%
if errorlevel 1 set ErrorMsg=Failed to compile %VITmp%
if not "%ErrorMsg%"=="" goto VerInfoRes_End
:VerInfoRes_End
if exist %VITmp% del %VITmp%
if not "%ErrorMsg%"=="" goto Error

goto Build_Pascal

rem ----------------------------------------------------------------------------
rem Build Pascal project
rem ----------------------------------------------------------------------------

:Build_Pascal
if not defined BuildPascal goto Build_End

echo.
echo BZInflate: Building Pascal Project
echo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
echo.

set PasBase=%Project%
set PasProj=%SrcDir%%PasBase%.dpr
set PasExe=%InstExeDir%%PasBase%.dll

echo Building %PasBase%.dpr
%DCC7% -B %PasProj%
if errorlevel 1 goto Pascal_Error
goto Pascal_End

:Pascal_Error
set ErrorMsg=Failed to compile %PasProj%
if exist %PascalExe% del %PasExe%
goto Error

:Pascal_End
goto Build_End


rem ----------------------------------------------------------------------------
rem Build completed
rem ----------------------------------------------------------------------------

:Build_End
goto End


rem ----------------------------------------------------------------------------
rem Handle errors
rem ----------------------------------------------------------------------------

:Error
echo *** Error: %ErrorMsg%
endlocal
exit /b 1


rem ----------------------------------------------------------------------------
rem Finished
rem ----------------------------------------------------------------------------

:End
echo.
echo BZInflate Done
endlocal
exit /b 0
