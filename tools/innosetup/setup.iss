;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
UsedUserAreasWarning=false
PrivilegesRequiredOverridesAllowed=commandline dialog
ChangesEnvironment=yes
#ifdef NELSON_WOA64
ArchitecturesAllowed=arm64
ArchitecturesInstallIn64BitMode=arm64
#endif
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
DefaultDirName={code:GetDefaultDirName}\{#APPLICATION_NAME}-{#APPLICATION_VERSION} ({#ARCH_LABEL})
DefaultGroupName={#APPLICATION_NAME}-{#APPLICATION_VERSION} ({#ARCH_LABEL})
UsePreviousAppDir=no
LicenseFile={#RootPath}lgpl-3.0.md
OutputDir={#RootPath}
SetupIconFile={#RootPath}resources/fibonacci.ico
UninstallDisplayIcon={app}\bin\{#BinPath}\{#APPLICATION_EXE_CLI_NAME}
Compression=lzma
SolidCompression=yes
OutputBaseFilename={#APPLICATION_NAME}-{#APPLICATION_VERSION}-{#ARCH_SUFFIX}
WizardStyle=modern
ChangesAssociations=yes
;==============================================================================
