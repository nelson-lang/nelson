;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of the Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
; Files Association (registry input)
;==============================================================================
;http://msdn2.microsoft.com/en-us/library/bb776870(VS.85).aspx
;http://msdn2.microsoft.com/en-us/library/bb776883.aspx
;http://msdn2.microsoft.com/en-us/library/bb776820.aspx
;==============================================================================
; Add a user environment variable with Nelson binaries path
;==============================================================================
Root: HKCU; Subkey: "Environment"; ValueType: string; ValueName: "NELSON_RUNTIME_PATH"; ValueData: {app}\bin\{#BinPath}; Check: not IsAdminInstallMode; Flags: uninsclearvalue noerror
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType: string; ValueName: "NELSON_RUNTIME_PATH"; ValueData: {app}\bin\{#BinPath}; Check: IsAdminInstallMode; Flags: uninsclearvalue noerror
;==============================================================================
#define ARGUMENT_ACTION_OPEN "-o"
#define ARGUMENT_ACTION_LOAD "-m"
#define ARGUMENT_ACTION_EXECUTE "-F"
;==============================================================================
;*.m
;==============================================================================
#define APPLICATION_EXTENSION_M "Nelson.m"
#define M_ENTRY ".m"
#define ICON_M_POS 1
;==============================================================================
Root: HKA; Subkey: "Software\Classes\{#M_ENTRY}\OpenWithProgids"; ValueType: string; ValueName: "{#APPLICATION_EXTENSION_M}"; ValueData: ""; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMFiles;Components: {#COMPONENT_IPC} and {#COMPONENT_GUI}  and {#COMPONENT_TEXT_EDITOR};
Root: HKA; Subkey: "Software\Classes\{#APPLICATION_EXTENSION_M}"; ValueType: string; ValueName: ""; ValueData: "{#APPLICATION_NAME} ({#M_ENTRY})"; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMFiles;Components:  {#COMPONENT_IPC} and {#COMPONENT_GUI}  and {#COMPONENT_TEXT_EDITOR};
Root: HKA; Subkey: "Software\Classes\{#APPLICATION_EXTENSION_M}\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\bin\{#BinPath}\Nelson-gui.exe,{#ICON_M_POS}"; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMFiles;Components: {#COMPONENT_IPC} and {#COMPONENT_GUI}  and {#COMPONENT_TEXT_EDITOR};
Root: HKA; Subkey: "Software\Classes\{#APPLICATION_EXTENSION_M}\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\bin\{#BinPath}\Nelson-gui.exe"" {#ARGUMENT_ACTION_EXECUTE} ""%1"""; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMFiles;Components: {#COMPONENT_IPC} and {#COMPONENT_GUI}  and {#COMPONENT_TEXT_EDITOR};
Root: HKA; Subkey: "Software\Classes\{#APPLICATION_EXTENSION_M}\shell\{cm:RegKeyRunWith} Nelson\command"; ValueType: string; ValueName: ""; ValueData: """{app}\bin\{#BinPath}\Nelson-gui.exe"" {#ARGUMENT_ACTION_OPEN} ""%1"""; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMFiles;Components: {#COMPONENT_IPC} and {#COMPONENT_GUI}  and {#COMPONENT_TEXT_EDITOR};
Root: HKA; Subkey: "Software\Classes\Applications\{#APPLICATION_NAME}\SupportedTypes"; ValueType: string; ValueName: "{#M_ENTRY}"; ValueData: ""; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMFiles;Components: {#COMPONENT_IPC} and {#COMPONENT_GUI}  and {#COMPONENT_TEXT_EDITOR};
;==============================================================================
;*.nh5
;==============================================================================
#define APPLICATION_EXTENSION_NH5 "Nelson.nh5"
#define NH5_ENTRY ".nh5"
#define ICON_NH5_POS 2
;==============================================================================
Root: HKA; Subkey: "Software\Classes\{#NH5_ENTRY}\OpenWithProgids"; ValueType: string; ValueName: "{#APPLICATION_EXTENSION_NH5}"; ValueData: ""; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateNh5Files;Components: {#COMPONENT_IPC} and {#COMPONENT_GUI};
Root: HKA; Subkey: "Software\Classes\{#APPLICATION_EXTENSION_NH5}"; ValueType: string; ValueName: ""; ValueData: "{#APPLICATION_NAME} ({#NH5_ENTRY})"; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateNh5Files;Components: {#COMPONENT_IPC} and {#COMPONENT_GUI};
Root: HKA; Subkey: "Software\Classes\{#APPLICATION_EXTENSION_NH5}\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\bin\{#BinPath}\Nelson-gui.exe,{#ICON_NH5_POS}"; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateNh5Files;Components: {#COMPONENT_IPC} and {#COMPONENT_GUI};
Root: HKA; Subkey: "Software\Classes\{#APPLICATION_EXTENSION_NH5}\shell\{cm:RegKeyLoadWith} Nelson\command"; ValueType: string; ValueName: ""; ValueData: """{app}\bin\{#BinPath}\Nelson-gui.exe"" {#ARGUMENT_ACTION_LOAD} ""%1"""; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateNh5Files;Components: {#COMPONENT_IPC} and {#COMPONENT_GUI};
Root: HKA; Subkey: "Software\Classes\Applications\{#APPLICATION_NAME}\SupportedTypes"; ValueType: string; ValueName: "{#NH5_ENTRY}"; ValueData: ""; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateNh5Files;Components: {#COMPONENT_IPC} and {#COMPONENT_GUI};
;==============================================================================
;*.mat
;==============================================================================
#define APPLICATION_EXTENSION_MAT "Nelson.mat"
#define MAT_ENTRY ".mat"
#define ICON_MAT_POS 3
;==============================================================================
Root: HKA; Subkey: "Software\Classes\{#MAT_ENTRY}\OpenWithProgids"; ValueType: string; ValueName: "{#APPLICATION_EXTENSION_MAT}"; ValueData: ""; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMatFiles;Components: {#COMPONENT_IPC} and {#COMPONENT_GUI};
Root: HKA; Subkey: "Software\Classes\{#APPLICATION_EXTENSION_MAT}"; ValueType: string; ValueName: ""; ValueData: "{#APPLICATION_NAME} ({#MAT_ENTRY})"; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMatFiles;Components: {#COMPONENT_IPC} and {#COMPONENT_GUI};
Root: HKA; Subkey: "Software\Classes\{#APPLICATION_EXTENSION_MAT}\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\bin\{#BinPath}\Nelson-gui.exe,{#ICON_MAT_POS}"; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMatFiles;Components: {#COMPONENT_IPC} and {#COMPONENT_GUI};
Root: HKA; Subkey: "Software\Classes\{#APPLICATION_EXTENSION_MAT}\shell\{cm:RegKeyLoadWith} Nelson\command"; ValueType: string; ValueName: ""; ValueData: """{app}\bin\{#BinPath}\Nelson-gui.exe"" {#ARGUMENT_ACTION_LOAD} ""%1"""; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMatFiles;Components: {#COMPONENT_IPC} and {#COMPONENT_GUI};
Root: HKA; Subkey: "Software\Classes\Applications\{#APPLICATION_NAME}\SupportedTypes"; ValueType: string; ValueName: "{#MAT_ENTRY}"; ValueData: ""; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMatFiles;Components: {#COMPONENT_IPC} and {#COMPONENT_GUI};
;==============================================================================
