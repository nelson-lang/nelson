@echo off
setlocal EnableExtensions DisableDelayedExpansion
if "%~2"=="" (
    echo ERROR: Expected Qt SDK and destination directories.
    exit /b 1
)
if not exist "%~1\bin\Qt6Core.dll" (
    echo ERROR: Qt6Core.dll not found in the Qt SDK.
    exit /b 1
)
rem The complete plugin/QML trees require the matching module inventory,
rem including new SDK modules and both release and debug variants.
robocopy "%~1\bin" "%~2" Qt6*.dll /IS /IT /R:0 /W:0 /NJH /NJS /NFL /NDL >nul
if errorlevel 8 (
    echo ERROR: Failed to copy the Qt runtime libraries.
    exit /b 1
)
exit /b 0
