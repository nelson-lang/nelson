;==============================================================================
; Copyright (c) 2016-2018 Allan CORNET (Nelson)
;==============================================================================
; LICENCE_BLOCK_BEGIN
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 2 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
; LICENCE_BLOCK_END
;==============================================================================
#ifdef NELSON_X64
Filename: "{app}\bin\{#BinPath}\vcredist_x64.exe"; Parameters: "/q /passive /norestart"; Check: VCRedistNeedsInstall; WorkingDir: {app}\bin\{#BinPath}; StatusMsg: Installing VC++ 2015 Redistributables...
#else
Filename: "{app}\bin\{#BinPath}\vcredist_x86.exe"; Parameters: "/q /passive /norestart"; Check: VCRedistNeedsInstall; WorkingDir: {app}\bin\{#BinPath}; StatusMsg: Installing VC++ 2015 Redistributables...
#endif
;==============================================================================
Filename: "{app}\bin\{#BinPath}\MSMpiSetup.exe"; Parameters: " -unattend -minimal"; WorkingDir: {app}\bin\{#BinPath}; StatusMsg: Installing MS-MPI Redistributables...
;==============================================================================
