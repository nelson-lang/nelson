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
#define CURRENT_YEAR "2026"
#define APPLICATION_VERSION "2.0.0"
#endif
#define APPLICATION_NAME "Nelson"
#define APPLICATION_EXE_GUI_NAME "Nelson-gui.exe"
#define APPLICATION_EXE_CLI_NAME "Nelson-cli.exe"
#define APPLICATION_EXE_ADV_CLI_NAME "Nelson-adv-cli.exe"
#define APPLICATION_EXE_WEBVIEW_NAME "nelson-webview.exe"
#define APPLICATION_EXE_LINT_NAME "Nelson-lint.exe"
#define APPLICATION_EXE_LSP_NAME "Nelson-lsp.exe"
#define APPLICATION_EXE_FORMAT_NAME "Nelson-format.exe"
#define APPLICATION_PUBLISHER "Nelson numerical software (Allan CORNET)"
;==============================================================================
; Architecture-specific defines (single source of truth)
;==============================================================================
#if defined(NELSON_WOA64)
  #define ARCH_LABEL "ARM64 bits"
  #define ARCH_SUFFIX "ARM64"
  #define BinPath "ARM64"
  #define ThirdpartyPath "nelson-thirdparty-arm64"
#elif defined(NELSON_X64)
  #define ARCH_LABEL "64 bits"
  #define ARCH_SUFFIX "x86-64"
  #define BinPath "x64"
  #define ThirdpartyPath "nelson-thirdparty-x64"
#else
  #error "Unsupported architecture: only x64 and ARM64 are supported (win32 is no longer supported)."
#endif
;==============================================================================
#define FULL_APPLICATION_NAME APPLICATION_NAME + "-" + APPLICATION_VERSION + " (" + ARCH_LABEL + ")"
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
