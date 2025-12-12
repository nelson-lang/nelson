@echo off
setlocal EnableDelayedExpansion

set MS_BUILD_PROFESSIONAL_PATH=C:\Program Files\Microsoft Visual Studio\2022\Professional\Msbuild\Current\Bin
set MS_BUILD_ENTERPRISE_PATH=C:\Program Files\Microsoft Visual Studio\2022\Enterprise\MSBuild\Current\Bin
set MS_BUILD_COMMUNITY_PATH=C:\Program Files\Microsoft Visual Studio\2022\Community\MSBuild\Current\Bin

:: check if argument is provided
if "%~1"=="" (
    echo Usage: build_windows.bat ^<x64^|win32^|arm64^>
    exit /b 1
)

set MSBUILD=
if exist "%MS_BUILD_PROFESSIONAL_PATH%" set MSBUILD=%MS_BUILD_PROFESSIONAL_PATH%
if exist "%MS_BUILD_ENTERPRISE_PATH%" set MSBUILD=%MS_BUILD_ENTERPRISE_PATH%
if exist "%MS_BUILD_COMMUNITY_PATH%" set MSBUILD=%MS_BUILD_COMMUNITY_PATH%

:: check if MSBuild is found
if "%MSBUILD%"=="" (
    echo Error: msbuild.exe not found in any known Visual Studio 2022 paths.
    exit /b 1
)

set "PATH=%MSBUILD%;%PATH%"

:: manage the argument
if /i "%~1"=="win64" (
    call win64-environment.bat env-only
    call "%MSBUILD%"\msbuild nelson.sln /p:Configuration=Release /p:Platform=x64 /target:"NelSon-gui" /m:4 /p:PreferredToolArchitecture=x64 /p:QTDIR="!QTDIR!" /p:PATH="!PATH!"
    exit /b !errorlevel!
)

if /i "%~1"=="win32" (
    call win32-environment.bat env-only
    call "%MSBUILD%"\msbuild nelson.sln /p:Configuration=Release /p:Platform=Win32 /target:"NelSon-gui" /m:4 /p:PreferredToolArchitecture=x64 /p:QTDIR="!QTDIR!" /p:PATH="!PATH!"
    exit /b !errorlevel!
)

if /i "%~1"=="arm64" (
    call ARM64-environment.bat env-only
    call "%MSBUILD%"\msbuild nelson.sln /p:Configuration=Release /p:Platform=ARM64 /target:"NelSon-gui" /m:4 /p:PreferredToolArchitecture=x64 /p:QTDIR="!QTDIR!" /p:PATH="!PATH!"
    exit /b !errorlevel!
)

:: case not handled
echo Unknown target: %1
exit /b 1