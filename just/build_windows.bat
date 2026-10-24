@echo off
setlocal EnableDelayedExpansion

:: Locate MSBuild in a Visual Studio version independent way using vswhere,
:: falling back to a few well known Visual Studio install paths.
set "VSWHERE=%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe"

set MS_BUILD_PROFESSIONAL_PATH=C:\Program Files\Microsoft Visual Studio\2026\Professional\Msbuild\Current\Bin
set MS_BUILD_ENTERPRISE_PATH=C:\Program Files\Microsoft Visual Studio\2026\Enterprise\MSBuild\Current\Bin
set MS_BUILD_COMMUNITY_PATH=C:\Program Files\Microsoft Visual Studio\2026\Community\MSBuild\Current\Bin

:: check if argument is provided
if "%~1"=="" (
    echo Usage: build_windows.bat ^<win64^|arm64^>
    exit /b 1
)

set "MSBUILD_EXE="

:: Resolve MSBuild via vswhere. Run vswhere directly (not inside a for /f, whose
:: quoting mangles the space-containing "Program Files (x86)" path) and read the
:: single-line result back from a temporary file.
if exist "%VSWHERE%" (
    "%VSWHERE%" -latest -requires Microsoft.Component.MSBuild -find "MSBuild\**\Bin\MSBuild.exe" > "%TEMP%\nelson_msbuild_path.txt" 2>nul
    set /p MSBUILD_EXE=<"%TEMP%\nelson_msbuild_path.txt"
    del "%TEMP%\nelson_msbuild_path.txt" >nul 2>&1
)

:: fallback to known Visual Studio paths when vswhere did not resolve MSBuild
if "%MSBUILD_EXE%"=="" (
    if exist "%MS_BUILD_PROFESSIONAL_PATH%\MSBuild.exe" set "MSBUILD_EXE=%MS_BUILD_PROFESSIONAL_PATH%\MSBuild.exe"
    if exist "%MS_BUILD_ENTERPRISE_PATH%\MSBuild.exe" set "MSBUILD_EXE=%MS_BUILD_ENTERPRISE_PATH%\MSBuild.exe"
    if exist "%MS_BUILD_COMMUNITY_PATH%\MSBuild.exe" set "MSBUILD_EXE=%MS_BUILD_COMMUNITY_PATH%\MSBuild.exe"
)

:: check if MSBuild is found
if "%MSBUILD_EXE%"=="" (
    echo Error: MSBuild.exe not found. Install Visual Studio with the C++ build tools.
    exit /b 1
)

:: put the MSBuild directory first on PATH
for %%d in ("%MSBUILD_EXE%") do set "MSBUILD_DIR=%%~dpd"
set "PATH=%MSBUILD_DIR%;%PATH%"

:: manage the argument
if /i "%~1"=="win64" (
    call .\win64-environment.bat env-only
    call "%MSBUILD_EXE%" nelson.sln /p:Configuration=Release /p:Platform=x64 /target:"NelSon-gui" /m /p:UseMultiToolTask=true /p:EnforceProcessCountAcrossBuilds=true /p:PreferredToolArchitecture=x64 /p:UseStructuredOutput=false /p:QTDIR="!QTDIR!" /p:PATH="!PATH!"
    exit /b !errorlevel!
)

if /i "%~1"=="arm64" (
    call .\ARM64-environment.bat env-only
    call "%MSBUILD_EXE%" nelson.sln /p:Configuration=Release /p:Platform=ARM64 /target:"NelSon-gui" /m /p:UseMultiToolTask=true /p:EnforceProcessCountAcrossBuilds=true /p:PreferredToolArchitecture=x64 /p:UseStructuredOutput=false /p:QTDIR="!QTDIR!" /p:PATH="!PATH!"
    exit /b !errorlevel!
)

:: case not handled
echo Unknown target: %1
exit /b 1
