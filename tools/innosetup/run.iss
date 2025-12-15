;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#ifndef NELSON_WOA64
Filename: "{app}\bin\{#BinPath}\MSMpiSetup.exe"; Parameters: " -unattend -minimal"; WorkingDir: "{app}\bin\{#BinPath}"; Check: IsAdminInstallMode; Components: {#COMPONENT_MPI};StatusMsg: Installing MS-MPI Redistributables...
#endif
;==============================================================================
