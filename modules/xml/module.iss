;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "xml"
;==============================================================================
; xml libraries
Source: {#RootPath}bin\{#BinPath}\iconv-2.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_XML};
Source: {#RootPath}bin\{#BinPath}\charset-1.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_XML};
Source: {#RootPath}bin\{#BinPath}\libxml2.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_XML};
Source: {#RootPath}bin\{#BinPath}\liblzma.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_XML};
Source: {#RootPath}bin\{#BinPath}\libxslt.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_XML};
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsXml.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_XML};
Source: {#RootPath}bin\{#BinPath}\libnlsXml_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_XML};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_XML};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_XML};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_XML};
;==============================================================================
;Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_XML};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.nhz; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_XML} and {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_XML} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.ref; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_XML} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.xml; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_XML} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.xsd; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_XML} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.xslt; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_XML} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
