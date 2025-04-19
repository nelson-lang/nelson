@echo off
setlocal enabledelayedexpansion

set MS_BUILD_PROFESSIONAL_PATH=C:\Program Files\Microsoft Visual Studio\2022\Professional\MSBuild\Current\Bin
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

set PATH=%MSBUILD%;%PATH%

:: manage the argument
if /i "%~1"=="win64" (
    set QTDIR64=%cd%\..\qt_windows_x64\6.9.0\msvc2022_64
    :: Set QTDIR to the value of QTDIR64
    set "QTDIR=!QTDIR64!
    :: Add QTDIR to PATH
    set "PATH=!QTDIR!;!PATH!"
    msbuild nelson.sln /p:Configuration=Release /p:Platform=x64 /target:"NelSon-gui" /m:4 /p:PreferredToolArchitecture=x64 
    goto :eof
)

if /i "%~1"=="win32" (
    set QTDIR32=%cd%\..\qt_windows_x86\5.15.12\msvc2019_32
    :: Set QTDIR to the value of QTDIR32
    set "QTDIR=!QTDIR32!
    :: Add QTDIR to PATH
    set "PATH=!QTDIR!;!PATH!"
    msbuild nelson.sln /p:Configuration=Release /p:Platform=Win32 /target:"NelSon-gui" /m:4 /p:PreferredToolArchitecture=x64
    goto :eof
)

if /i "%~1"=="arm64" (
    echo not implemented yet
    goto :eof
)

:: case not handled
echo Unknown target: %1
exit /b 1
