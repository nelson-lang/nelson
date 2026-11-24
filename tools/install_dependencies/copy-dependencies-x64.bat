@echo off
rem =============================================================================
rem Copyright (c) 2016-present Allan CORNET (Nelson)
rem =============================================================================
rem This file is part of Nelson.
rem =============================================================================
rem # LICENCE_BLOCK_BEGIN
rem # SPDX-License-Identifier: LGPL-3.0-or-later
rem # LICENCE_BLOCK_END
rem =============================================================================
rem Scope all working variables so they do not leak into the caller's
rem environment. In particular this script uses a "rc" variable for robocopy
rem exit codes; without setlocal it would export RC=<code> (env vars are
rem case-insensitive on Windows), which cmake later reads as the resource
rem compiler (CMAKE_RC_COMPILER) and fails with "Could not find the compiler
rem specified in the environment variable RC".
setlocal EnableExtensions

rem Resolve THIRD_PARTY__DIR to absolute path
pushd "%~dp0..\..\..\nelson-thirdparty-x64" 2>nul
if errorlevel 1 (
    echo ERROR: Third-party directory not found: "%~dp0..\..\..\nelson-thirdparty-x64"
    exit /b 1
)
set "THIRD_PARTY__DIR=%CD%"
popd

rem Resolve BIN_DIR to absolute path
pushd "%~dp0..\..\bin\x64" 2>nul
if errorlevel 1 (
    echo ERROR: Bin directory not found: "%~dp0..\..\bin\x64"
    exit /b 1
)
set "BIN_DIR=%CD%"
popd

rem =============================================================================
rem Install dll files in bin directory
rem robocopy exit codes 0-7 are informational (success), 8+ indicate errors

set "SRC=%THIRD_PARTY__DIR%\arpack-ng\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy arpack-ng DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\curl\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy curl DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\libgif\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy libgif DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\libpng\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy libpng DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\libwebp\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy libwebp DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\hdf5\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy hdf5 DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\icu\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy icu DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\libffi\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy libffi DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\libgit2\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy libgit2 DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\libsndfile\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy libsndfile DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\libxml2\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy libxml2 DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\libxslt\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy libxslt DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\matio\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy matio DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\netcdf\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy netcdf DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\openblas\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy openblas DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\pcre\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy pcre DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\portaudio\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy portaudio DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\sundials\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy sundials DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\highs\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy highs DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\qhull\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy qhull DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\arrow\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy arrow DLLs (robocopy rc=%rc%) & exit /b 1 )


set "SRC=%THIRD_PARTY__DIR%\taglib\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy taglib DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\tiff\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy tiff DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\libwebp\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy libwebp DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\libpng\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy libpng DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\libjpeg-turbo\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy libjpeg-turbo DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\umfpack\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy umfpack DLLs (robocopy rc=%rc%) & exit /b 1 )

set "SRC=%THIRD_PARTY__DIR%\zlib\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy zlib DLLs (robocopy rc=%rc%) & exit /b 1 )

rem zstd is optional: without it, ZIP Zstandard entries are not supported.
set "SRC=%THIRD_PARTY__DIR%\zstd\bin"
if exist "%SRC%\" (
    robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
    if errorlevel 8 ( echo ERROR: Failed to copy zstd DLLs & exit /b 1 )
) else (
    echo WARNING: zstd folder not found, ZIP Zstandard entries disabled: "%SRC%"
)

rem libarchive: copied after arrow and tiff so its newer liblzma and lz4 win.
set "SRC=%THIRD_PARTY__DIR%\libarchive\bin"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy libarchive DLLs (robocopy rc=%rc%) & exit /b 1 )

rem MS-MPI redistributable installer - shipped next to the binaries and run by the
rem installer (tools/innosetup/run.iss) to deploy the MS-MPI runtime. The exe lives
rem directly in the mpi third-party folder, not in a bin subdirectory.
set "SRC=%THIRD_PARTY__DIR%\mpi"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" MSMpiSetup.exe /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy MS-MPI installer (robocopy rc=%rc%) & exit /b 1 )

rem dawn (WebGPU) - optional dependency of the gpu_engine module; skip if absent
set "SRC=%THIRD_PARTY__DIR%\dawn\bin"
if not exist "%SRC%\" (
    echo INFO: dawn not found in "%SRC%", skipping optional gpu_engine dependency.
    goto :dawn_done
)
robocopy "%SRC%" "%BIN_DIR%" webgpu_dawn.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy dawn DLLs (robocopy rc=%rc%) & exit /b 1 )
:dawn_done

rem WebView2 loader - required by the web_engine module to host nflow/open_system in
rem an embedded WebView2 window. WebUI resolves it at runtime with
rem LoadLibraryA("WebView2Loader.dll"), so it must sit next to the binaries; the
rem system-wide WebView2 Runtime alone is NOT enough. Without it WebUI silently
rem falls back to an app-mode browser window. Optional; skip if absent.
set "SRC=%THIRD_PARTY__DIR%\webview\bin"
if not exist "%SRC%\" (
    echo INFO: webview not found in "%SRC%", skipping optional WebView2 loader.
    goto :webview_done
)
robocopy "%SRC%" "%BIN_DIR%" WebView2Loader.dll /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy WebView2 loader (robocopy rc=%rc%) & exit /b 1 )
:webview_done

rem visual c++ runtime
set "SRC=%THIRD_PARTY__DIR%\vc14.5-runtime"
if not exist "%SRC%\" (
    echo ERROR: Source folder not found: "%SRC%"
    exit /b 1
)
robocopy "%SRC%" "%BIN_DIR%" *.dll vc_redist.x64.exe /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 ( echo ERROR: Failed to copy vc14.5-runtime files (robocopy rc=%rc%) & exit /b 1 )
rem =============================================================================
rem Copy optional tooling (clang-format / cmake) into nelson/tools
rem Resolve NELSON_DIR to absolute path
pushd "%~dp0\..\.." 2>nul
if %ERRORLEVEL% GEQ 1 (
    echo WARNING: Nelson directory not found: "%~dp0\..\.."
    set "NELSON_DIR="
) else (
    set "NELSON_DIR=%CD%"
    popd
)

rem ClangFormat
echo Copying clang-format runtime/tools...
if not defined NELSON_DIR goto :skip_clangformat_nelson
if exist "%THIRD_PARTY__DIR%\clang-format\" goto :copy_clangformat
echo Skipping clang-format (not available in third-party sources).
goto :after_clangformat

:copy_clangformat
if not exist "%NELSON_DIR%\tools\clang-format\" (
    mkdir "%NELSON_DIR%\tools\clang-format" >nul 2>&1
)
xcopy /E /I /Y "%THIRD_PARTY__DIR%\clang-format" "%NELSON_DIR%\tools\clang-format" >nul 2>&1
goto :after_clangformat

:skip_clangformat_nelson
echo Skipping clang-format (NELSON_DIR not resolved).

:after_clangformat

rem CMake helper files
echo Copying cmake helper files...
if not defined NELSON_DIR goto :skip_cmake_nelson
if exist "%THIRD_PARTY__DIR%\cmake\" goto :copy_cmake
echo Skipping cmake helper files (not available in third-party sources).
goto :after_cmake

:copy_cmake
if not exist "%NELSON_DIR%\tools\cmake\" (
    mkdir "%NELSON_DIR%\tools\cmake" >nul 2>&1
)
rem Keep stdout quiet (long file list) but let copy errors surface on stderr.
xcopy /E /I /Y "%THIRD_PARTY__DIR%\cmake" "%NELSON_DIR%\tools\cmake" >nul
if errorlevel 1 echo WARNING: xcopy reported an error while copying cmake helper files.
rem The share directory holds cmake's Modules; without it cmake fails at runtime
rem with "Could not find CMAKE_ROOT". Verify the copy actually brought it over.
if not exist "%NELSON_DIR%\tools\cmake\bin\cmake.exe" echo WARNING: cmake executable missing after copy: "%NELSON_DIR%\tools\cmake\bin\cmake.exe".
if not exist "%NELSON_DIR%\tools\cmake\share\" echo WARNING: cmake share directory missing after copy - dlmake/mex will fail with "Could not find CMAKE_ROOT". Check the third-party source "%THIRD_PARTY__DIR%\cmake\share".
goto :after_cmake

:skip_cmake_nelson
echo Skipping cmake helper files (NELSON_DIR not resolved).

:after_cmake

rem MKL blas, lapack, FFTW, VML wrapper
echo Copying MKL wrapper libraries...
set "MKL_SRC=%THIRD_PARTY__DIR%\mkl"
rem NOTE: keep these as two single-line guards. A parenthesized if-block with an
rem echo message containing literal parentheses breaks cmd's block parsing and
rem makes the goto run unconditionally (silently skipping the whole MKL copy).
if not exist "%MKL_SRC%\" echo Skipping MKL: source not found "%MKL_SRC%"
if not exist "%MKL_SRC%\" goto :after_mkl
if not exist "%BIN_DIR%\mkl\" mkdir "%BIN_DIR%\mkl" >nul 2>&1

if exist "%MKL_SRC%\mkl_license.txt" (
    copy "%MKL_SRC%\mkl_license.txt" "%BIN_DIR%\mkl\mkl_license.txt" >nul 2>&1
    copy "%MKL_SRC%\mkl_license.txt" "%BIN_DIR%\mkl_license.txt" >nul 2>&1
)

set "ROBO_SRC=%MKL_SRC%\blas-lapack-wrapper\bin"
set "ROBO_DST=%BIN_DIR%\mkl"
set "ROBO_PAT=*.dll"
set "ROBO_MSG=Failed to copy MKL blas-lapack DLLs"
call :_robocopy_env

set "ROBO_SRC=%MKL_SRC%\blas-lapack-wrapper\lib"
set "ROBO_DST=%BIN_DIR%\mkl"
set "ROBO_PAT=*.lib"
set "ROBO_MSG=Failed to copy MKL blas-lapack libs"
call :_robocopy_env

set "ROBO_SRC=%MKL_SRC%\fftw-wrapper\bin"
set "ROBO_DST=%BIN_DIR%\mkl"
set "ROBO_PAT=*.dll"
set "ROBO_MSG=Failed to copy MKL fftw DLLs"
call :_robocopy_env

set "ROBO_SRC=%MKL_SRC%\omp\bin"
set "ROBO_DST=%BIN_DIR%\mkl"
set "ROBO_PAT=*.dll"
set "ROBO_MSG=Failed to copy MKL omp DLLs"
call :_robocopy_env

set "ROBO_SRC=%MKL_SRC%\vml\bin"
set "ROBO_DST=%BIN_DIR%\mkl"
set "ROBO_PAT=*.dll"
set "ROBO_MSG=Failed to copy MKL vml DLLs"
call :_robocopy_env

set "ROBO_SRC=%MKL_SRC%\blas-lapack-wrapper\bin"
set "ROBO_DST=%BIN_DIR%"
set "ROBO_PAT=*.dll"
set "ROBO_MSG=Failed to copy MKL blas-lapack DLLs to root"
call :_robocopy_env

set "ROBO_SRC=%MKL_SRC%\blas-lapack-wrapper\lib"
set "ROBO_DST=%BIN_DIR%"
set "ROBO_PAT=*.lib"
set "ROBO_MSG=Failed to copy MKL blas-lapack libs to root"
call :_robocopy_env

set "ROBO_SRC=%MKL_SRC%\fftw-wrapper\bin"
set "ROBO_DST=%BIN_DIR%"
set "ROBO_PAT=*.dll"
set "ROBO_MSG=Failed to copy MKL fftw DLLs to root"
call :_robocopy_env

set "ROBO_SRC=%MKL_SRC%\omp\bin"
set "ROBO_DST=%BIN_DIR%"
set "ROBO_PAT=*.dll"
set "ROBO_MSG=Failed to copy MKL omp DLLs to root"
call :_robocopy_env

set "ROBO_SRC=%MKL_SRC%\vml\bin"
set "ROBO_DST=%BIN_DIR%"
set "ROBO_PAT=*.dll"
set "ROBO_MSG=Failed to copy MKL vml DLLs to root"
call :_robocopy_env

:after_mkl

rem =============================================================================
rem End of the main flow. Terminate here so execution does NOT fall through into
rem the subroutine definitions below: without this, control would enter
rem :_robocopy with no arguments and run robocopy with empty source/dest,
rem failing with rc=16 ("Robocopy failed ... source=\"\" dest=\"\"").
exit /b 0

rem helper: robust robocopy wrapper
:_robocopy
rem %1 = source folder, %2 = destination folder, %3 = file pattern, %4 = optional error message
if not exist "%~1\" (
    rem nothing to copy
    goto :eof
)
robocopy "%~1" "%~2" %~3 /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 (
    if "%~4"=="" (
        echo ERROR: Robocopy failed rc=%rc% source="%~1" dest="%~2"
    ) else (
        echo ERROR: %~4 - robocopy rc=%rc% source="%~1" dest="%~2"
    )
    exit /b %rc%
)
goto :eof

exit /b 0

:_robocopy_env
rem env-driven robocopy helper: expects ROBO_SRC, ROBO_DST, ROBO_PAT, ROBO_MSG
if not defined ROBO_SRC goto :eof
if not exist "%ROBO_SRC%\" goto :eof
robocopy "%ROBO_SRC%" "%ROBO_DST%" %ROBO_PAT% /NJH /NJS /NFL /NDL >nul 2>&1
set rc=%ERRORLEVEL%
if %rc% GEQ 8 (
    if not defined ROBO_MSG (
        echo ERROR: Robocopy failed rc=%rc% source="%ROBO_SRC%" dest="%ROBO_DST%"
    ) else (
        echo ERROR: %ROBO_MSG% - robocopy rc=%rc% source="%ROBO_SRC%" dest="%ROBO_DST%"
    )
    exit /b %rc%
)
goto :eof
