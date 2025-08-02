;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#ifdef NELSON_X64
Filename: "{app}\bin\{#BinPath}\vc_redist.x64.exe"; Parameters: "/q /passive /norestart"; Check: VCRedistNeedsInstall; WorkingDir: "{app}\bin\{#BinPath}"; StatusMsg: Installing VC++ 2022 Redistributables...
#else
Filename: "{app}\bin\{#BinPath}\vc_redist.x86.exe"; Parameters: "/q /passive /norestart"; Check: VCRedistNeedsInstall; WorkingDir: "{app}\bin\{#BinPath}"; StatusMsg: Installing VC++ 2022 Redistributables...
#endif
;==============================================================================
Filename: "{app}\bin\{#BinPath}\MSMpiSetup.exe"; Parameters: " -unattend -minimal"; WorkingDir: "{app}\bin\{#BinPath}"; Check: IsAdminInstallMode; Components: {#COMPONENT_MPI};StatusMsg: Installing MS-MPI Redistributables...
;==============================================================================
