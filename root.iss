;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of the Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU Lesser General Public
; License as published by the Free Software Foundation; either
; version 2.1 of the License, or (at your option) any later version.
;
; Alternatively, you can redistribute it and/or
; modify it under the terms of the GNU General Public License as
; published by the Free Software Foundation; either version 2 of
; the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU Lesser General Public License for more details.
;
; You should have received a copy of the GNU Lesser General Public
; License along with this program. If not, see <http://www.gnu.org/licenses/>.
; LICENCE_BLOCK_END
;==============================================================================
Source: {#RootPath}COPYING; DestDir: {app}\
Source: {#RootPath}COPYING.LGPLv2.1; DestDir: {app}\
Source: {#RootPath}LICENSE; DestDir: {app}\
Source: {#RootPath}CONTRIBUTORS.md; DestDir: {app}\
Source: {#RootPath}README.md; DestDir: {app}\
Source: {#RootPath}THIRDPARTY.md; DestDir: {app}\
Source: {#RootPath}CHANGELOG.md; DestDir: {app}\
Source: {#RootPath}CHANGELOG-0.4.x.md; DestDir: {app}\
Source: {#RootPath}CHANGELOG-0.3.x.md; DestDir: {app}\
Source: {#RootPath}CHANGELOG-0.2.x.md; DestDir: {app}\
Source: {#RootPath}CHANGELOG-0.1.x.md; DestDir: {app}\
Source: {#RootPath}CLA.md; DestDir: {app}\
;==============================================================================
#define MODULE_NAME "module_skeleton"
;==============================================================================
Source: {#RootPath}\{#MODULE_NAME}\loader.nls; DestDir: {app}\{#MODULE_NAME}\; Flags: skipifsourcedoesntexist
Source: {#RootPath}\{#MODULE_NAME}\builder.nls; DestDir: {app}\{#MODULE_NAME}\; Flags: skipifsourcedoesntexist
Source: {#RootPath}\{#MODULE_NAME}\module.json; DestDir: {app}\{#MODULE_NAME}\; Flags: skipifsourcedoesntexist
;==============================================================================
Source: {#RootPath}\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\{#MODULE_NAME}\etc\; Flags: skipifsourcedoesntexist
Source: {#RootPath}\{#MODULE_NAME}\etc\finish.nls; DestDir: {app}\{#MODULE_NAME}\etc\; Flags: skipifsourcedoesntexist
;==============================================================================
Source: {#RootPath}\{#MODULE_NAME}\functions\*.m; DestDir: {app}\{#MODULE_NAME}\functions\; Flags: skipifsourcedoesntexist
;==============================================================================
Source: {#RootPath}\{#MODULE_NAME}\help\en_US\images\*.png; DestDir: {app}\{#MODULE_NAME}\help\en_US\images\; Flags: skipifsourcedoesntexist
Source: {#RootPath}\{#MODULE_NAME}\help\en_US\xml\*.xml; DestDir: {app}\{#MODULE_NAME}\help\en_US\xml\; Flags: skipifsourcedoesntexist
;==============================================================================
Source: {#RootPath}\{#MODULE_NAME}\src\*.*; DestDir: {app}\{#MODULE_NAME}\src\; Flags: recursesubdirs skipifsourcedoesntexist
Source: {#RootPath}\{#MODULE_NAME}\builtin\*.*; DestDir: {app}\{#MODULE_NAME}\builtin\; Flags: recursesubdirs skipifsourcedoesntexist
;==============================================================================
Source: {#RootPath}\{#MODULE_NAME}\tests\*.nls; DestDir: {app}\{#MODULE_NAME}\tests\; Flags: recursesubdirs skipifsourcedoesntexist
;==============================================================================
