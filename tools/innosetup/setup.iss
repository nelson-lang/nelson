;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of the Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
UsedUserAreasWarning=false
PrivilegesRequiredOverridesAllowed=commandline dialog
ChangesEnvironment=yes
#ifdef NELSON_X64
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
#endif
AppId={{2ED97A2C-CAA6-467F-82D9-02FC564A8950}
AppName={#APPLICATION_NAME}
AppVersion={#APPLICATION_VERSION}
VersionInfoVersion={#APPLICATION_VERSION}
AppMutex={#APPLICATION_NAME}-{#APPLICATION_VERSION}
AppPublisher={#APPLICATION_PUBLISHER}
AppVerName={#FULL_APPLICATION_NAME}
#ifdef NELSON_X64
DefaultDirName={code:GetDefaultDirName}\{#APPLICATION_NAME}-{#APPLICATION_VERSION} (64 bits)
DefaultGroupName={#APPLICATION_NAME}-{#APPLICATION_VERSION} (64 bits)
#else
DefaultDirName={code:GetDefaultDirName}\{#APPLICATION_NAME}-{#APPLICATION_VERSION} (32 bits)
DefaultGroupName={#APPLICATION_NAME}-{#APPLICATION_VERSION} (32 bits)
#endif
UsePreviousAppDir=no
LicenseFile={#RootPath}lgpl-3.0.md
OutputDir={#RootPath}
SetupIconFile={#RootPath}resources/fibonacci.ico
UninstallDisplayIcon={app}\bin\{#BinPath}\{#APPLICATION_EXE_CLI_NAME}
Compression=lzma
SolidCompression=yes
#ifdef NELSON_X64
OutputBaseFilename={#APPLICATION_NAME}-{#APPLICATION_VERSION}-x86-64
#else
OutputBaseFilename={#APPLICATION_NAME}-{#APPLICATION_VERSION}-x86-32
#endif
WizardStyle=modern
ChangesAssociations=yes
;==============================================================================
