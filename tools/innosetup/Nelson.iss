;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#include "generated.iss"
;==============================================================================
#ifndef GENERATED_INFO
#define NELSON_X64
#define WITH_JULIA_ENGINE
#define NELSON_DEBUG
#define CURRENT_YEAR "2025"
#define APPLICATION_VERSION "1.12.0"
#endif
#define APPLICATION_NAME "Nelson"
#define APPLICATION_EXE_GUI_NAME "Nelson-gui.exe"
#define APPLICATION_EXE_CLI_NAME "Nelson-cli.exe"
#define APPLICATION_EXE_ADV_CLI_NAME "Nelson-adv-cli.exe"
#define APPLICATION_PUBLISHER "Nelson numerical software (Allan CORNET)"
#if defined(NELSON_X64) || defined(NELSON_WOA64)
#define FULL_APPLICATION_NAME APPLICATION_NAME + "-" + APPLICATION_VERSION + " (64 bits)"
#else
#define FULL_APPLICATION_NAME APPLICATION_NAME + "-" + APPLICATION_VERSION + " (32 bits)"
#endif
#if defined(NELSON_WOA64)
    #ifdef NELSON_DEBUG
        #define BOOST_TARGET  "vc145-mt-gd-a64-1_89"
    #else
        #define BOOST_TARGET  "vc145-mt-a64-1_89"
    #endif
#else
#if defined(NELSON_X64)
#ifdef NELSON_DEBUG
#define BOOST_TARGET  "vc143-mt-gd-x64-1_89"
#else
#define BOOST_TARGET  "vc143-mt-x64-1_89"
#endif
#else
#ifdef NELSON_DEBUG
#define BOOST_TARGET  "vc143-mt-gd-x32-1_89"
#else
#define BOOST_TARGET  "vc143-mt-x32-1_89"
#endif
#endif
#endif
;==============================================================================
#define RootPath "../../"
#if defined(NELSON_WOA64)
    #define BinPath "ARM64"
#else
    #if defined(NELSON_X64)
        #define BinPath "x64"
    #else
        #define BinPath "win32"
    #endif
#endif    
;==============================================================================
[Setup]
#include "setup.iss"
;==============================================================================
[Languages]
#include "languages.iss"
;==============================================================================
[CustomMessages]
#include "custommessages.iss"
;==============================================================================
[Types]
#include "types.iss"
;==============================================================================
[Components]
#include "components.iss"
;==============================================================================
[Tasks]
#include "tasks.iss"
;==============================================================================
[Files]
#include "files.iss"
;==============================================================================
[Icons]
#include "icons.iss"
;==============================================================================
[Registry]
#include "registry.iss"
;==============================================================================
[Run]
#include "run.iss"
;==============================================================================
[UninstallDelete]
#include "uninstalldelete.iss"
;==============================================================================
[Code]
#include "code.iss"
