;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
Name: "{group}\{#FULL_APPLICATION_NAME} GUI"; Filename: "{app}\bin\{#BinPath}\{#APPLICATION_EXE_GUI_NAME}";Components: {#COMPONENT_GUI};
Name: "{group}\{#FULL_APPLICATION_NAME} CLI"; Filename: "{app}\bin\{#BinPath}\{#APPLICATION_EXE_CLI_NAME}"
Name: "{group}\{#FULL_APPLICATION_NAME} Advanced CLI"; Filename: "{app}\bin\{#BinPath}\{#APPLICATION_EXE_ADV_CLI_NAME}";Components: {#COMPONENT_GUI};
Name: "{group}\{cm:UninstallProgram,{#FULL_APPLICATION_NAME}}"; Filename: "{uninstallexe}"
;
Name: "{commondesktop}\{#FULL_APPLICATION_NAME}"; Filename: "{app}\bin\{#BinPath}\{#APPLICATION_EXE_GUI_NAME}"; Tasks: desktopicon;Components: {#COMPONENT_GUI}; Check: IsAdminInstallMode
Name: "{commondesktop}\{#FULL_APPLICATION_NAME}"; Filename: "{app}\bin\{#BinPath}\{#APPLICATION_EXE_CLI_NAME}"; Tasks: desktopicon;Components: not {#COMPONENT_GUI}; Check: IsAdminInstallMode
;
Name: "{userdesktop}\{#FULL_APPLICATION_NAME}"; Filename: "{app}\bin\{#BinPath}\{#APPLICATION_EXE_GUI_NAME}"; Tasks: desktopicon;Components: {#COMPONENT_GUI}; Check: Not IsAdminInstallMode
Name: "{userdesktop}\{#FULL_APPLICATION_NAME}"; Filename: "{app}\bin\{#BinPath}\{#APPLICATION_EXE_CLI_NAME}"; Tasks: desktopicon;Components: not {#COMPONENT_GUI}; Check: Not IsAdminInstallMode
;==============================================================================
