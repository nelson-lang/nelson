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
;==============================================================================
; Architecture-specific defines (single source of truth)
;==============================================================================
#if defined(NELSON_WOA64)
  #define ARCH_LABEL "ARM64 bits"
  #define ARCH_SUFFIX "ARM64"
  #define BinPath "ARM64"
  #define BOOST_COMPILER "vc145"
  #define BOOST_ARCH "a64"
#elif defined(NELSON_X64)
  #define ARCH_LABEL "64 bits"
  #define ARCH_SUFFIX "x86-64"
  #define BinPath "x64"
  #define BOOST_COMPILER "vc143"
  #define BOOST_ARCH "x64"
#else
  #define ARCH_LABEL "32 bits"
  #define ARCH_SUFFIX "x86-32"
  #define BinPath "win32"
  #define BOOST_COMPILER "vc143"
  #define BOOST_ARCH "x32"
#endif
;==============================================================================
#define FULL_APPLICATION_NAME APPLICATION_NAME + "-" + APPLICATION_VERSION + " (" + ARCH_LABEL + ")"
;==============================================================================
#define BOOST_VERSION "1_89"
#ifdef NELSON_DEBUG
  #define BOOST_TARGET BOOST_COMPILER + "-mt-gd-" + BOOST_ARCH + "-" + BOOST_VERSION
#else
  #define BOOST_TARGET BOOST_COMPILER + "-mt-" + BOOST_ARCH + "-" + BOOST_VERSION
#endif
;==============================================================================
#define RootPath "../../"
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
