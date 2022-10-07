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
Root: HKCR; Subkey: {#M_ENTRY}; ValueType: string; ValueData: {#APPLICATION_EXTENSION_M}; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMFiles;
Root: HKCR; Subkey: {#M_ENTRY}\ShellNew; ValueName : NullFile ;ValueType: string; ValueData: ""; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMFiles;
Root: HKCR; Subkey: {#APPLICATION_EXTENSION_M}; ValueType: string; ValueData: "{#APPLICATION_NAME} Application ({#APPLICATION_EXTENSION_M})"; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMFiles;
Root: HKCR; Subkey: {#APPLICATION_EXTENSION_M}\DefaultIcon; ValueType: string; ValueData: {app}\bin\{#BinPath}\Nelson-gui.exe,{#ICON_M_POS}; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMFiles;
Root: HKCR; Subkey: {#APPLICATION_EXTENSION_M}\shell\open\command; ValueType: string; ValueData: """{app}\bin\{#BinPath}\Nelson-gui.exe"" {#ARGUMENT_ACTION_OPEN} ""%1"""; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMFiles;Components: {#COMPONENT_TEXT_EDITOR};
Root: HKCR; Subkey: {#APPLICATION_EXTENSION_M}\shell\{cm:RegKeyRunWith} Nelson\command; ValueType: string; ValueData: """{app}\bin\{#BinPath}\Nelson-gui.exe"" {#ARGUMENT_ACTION_EXECUTE} ""%1"""; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMFiles;
;==============================================================================
;*.nh5
;==============================================================================
#define APPLICATION_EXTENSION_NH5 "Nelson.nh5"
#define NH5_ENTRY ".nh5"
#define ICON_NH5_POS 2
;==============================================================================
Root: HKCR; Subkey: {#NH5_ENTRY}; ValueType: string; ValueData: {#APPLICATION_EXTENSION_NH5}; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateNh5Files;
Root: HKCR; Subkey: {#NH5_ENTRY}\ShellNew; ValueName : NullFile ;ValueType: string; ValueData: ""; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateNh5Files;
Root: HKCR; Subkey: {#APPLICATION_EXTENSION_NH5}; ValueType: string; ValueData: "{#APPLICATION_NAME} Application ({#APPLICATION_EXTENSION_NH5})"; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateNh5Files;
Root: HKCR; Subkey: {#APPLICATION_EXTENSION_NH5}\DefaultIcon; ValueType: string; ValueData: {app}\bin\{#BinPath}\Nelson-gui.exe,{#ICON_NH5_POS}; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateNh5Files;
Root: HKCR; Subkey: {#APPLICATION_EXTENSION_NH5}\shell\{cm:RegKeyLoadWith} Nelson\command; ValueType: string; ValueData: """{app}\bin\{#BinPath}\Nelson-gui.exe"" {#ARGUMENT_ACTION_LOAD} ""%1"""; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateNh5Files;
;==============================================================================
;*.mat
;==============================================================================
#define APPLICATION_EXTENSION_MAT "Nelson.mat"
#define MAT_ENTRY ".mat"
#define ICON_MAT_POS 3
;==============================================================================
Root: HKCR; Subkey: {#MAT_ENTRY}; ValueType: string; ValueData: {#APPLICATION_EXTENSION_MAT}; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMatFiles;
Root: HKCR; Subkey: {#MAT_ENTRY}\ShellNew; ValueName : NullFile ;ValueType: string; ValueData: ""; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMatFiles;
Root: HKCR; Subkey: {#APPLICATION_EXTENSION_MAT}; ValueType: string; ValueData: "{#APPLICATION_NAME} Application ({#APPLICATION_EXTENSION_MAT})"; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMatFiles;
Root: HKCR; Subkey: {#APPLICATION_EXTENSION_MAT}\DefaultIcon; ValueType: string; ValueData: {app}\bin\{#BinPath}\Nelson-gui.exe,{#ICON_MAT_POS}; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMatFiles;
Root: HKCR; Subkey: {#APPLICATION_EXTENSION_MAT}\shell\{cm:RegKeyLoadWith} Nelson\command; ValueType: string; ValueData: """{app}\bin\{#BinPath}\Nelson-gui.exe"" {#ARGUMENT_ACTION_LOAD} ""%1"""; Flags: deletekey uninsdeletekey noerror; Tasks: AssociateMatFiles;
;==============================================================================
