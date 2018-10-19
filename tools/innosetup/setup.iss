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
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64
#endif

AppId={{2ED97A2C-CAA6-467F-82D9-02FC564A8950}
AppName={#APPLICATION_NAME}
AppVersion={#APPLICATION_VERSION}
AppMutex={#APPLICATION_NAME}-{#APPLICATION_VERSION}
AppPublisher={#APPLICATION_PUBLISHER}
AppVerName={#FULL_APPLICATION_NAME}
#ifdef NELSON_X64
DefaultDirName={pf}\{#APPLICATION_NAME}-{#APPLICATION_VERSION} (64 bits)
DefaultGroupName={#APPLICATION_NAME}-{#APPLICATION_VERSION} (64 bits)
#else
DefaultDirName={pf}\{#APPLICATION_NAME}-{#APPLICATION_VERSION} (32 bits)
DefaultGroupName={#APPLICATION_NAME}-{#APPLICATION_VERSION} (32 bits)
#endif

UsePreviousAppDir=no
LicenseFile={#RootPath}COPYING.md
OutputDir={#RootPath}
SetupIconFile={#RootPath}resources/fibonacci.ico
Compression=lzma
SolidCompression=yes

#ifdef NELSON_X64
OutputBaseFilename={#APPLICATION_NAME}-{#APPLICATION_VERSION}-x86-64
#else
OutputBaseFilename={#APPLICATION_NAME}-{#APPLICATION_VERSION}-x86-32
#endif

;==============================================================================
