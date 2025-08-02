;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
Source: {#RootPath}gpl-3.0.md; DestDir: {app}\
Source: {#RootPath}lgpl-3.0.md; DestDir: {app}\
Source: {#RootPath}CONTRIBUTORS.md; DestDir: {app}\
Source: {#RootPath}README.md; DestDir: {app}\
Source: {#RootPath}THIRDPARTY.md; DestDir: {app}\
Source: {#RootPath}CHANGELOG.md; DestDir: {app}\
Source: {#RootPath}CHANGELOG-0.7.x.md; DestDir: {app}\
Source: {#RootPath}CHANGELOG-0.6.x.md; DestDir: {app}\
Source: {#RootPath}CHANGELOG-0.5.x.md; DestDir: {app}\
Source: {#RootPath}CHANGELOG-0.4.x.md; DestDir: {app}\
Source: {#RootPath}CHANGELOG-0.3.x.md; DestDir: {app}\
Source: {#RootPath}CHANGELOG-0.2.x.md; DestDir: {app}\
Source: {#RootPath}CHANGELOG-0.1.x.md; DestDir: {app}\
Source: {#RootPath}CLA.md; DestDir: {app}\
;==============================================================================
#define MODULE_NAME "module_skeleton"
;==============================================================================
Source: {#RootPath}\{#MODULE_NAME}\loader.m; DestDir: {app}\{#MODULE_NAME}\; Flags: skipifsourcedoesntexist;Components: {#COMPONENT_MODULE_SKELETON};
Source: {#RootPath}\{#MODULE_NAME}\builder.m; DestDir: {app}\{#MODULE_NAME}\; Flags: skipifsourcedoesntexist;Components: {#COMPONENT_MODULE_SKELETON};
Source: {#RootPath}\{#MODULE_NAME}\module.json; DestDir: {app}\{#MODULE_NAME}\; Flags: skipifsourcedoesntexist;Components: {#COMPONENT_MODULE_SKELETON};
;==============================================================================
Source: {#RootPath}\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\{#MODULE_NAME}\etc\; Flags: skipifsourcedoesntexist;Components: {#COMPONENT_MODULE_SKELETON};
Source: {#RootPath}\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\{#MODULE_NAME}\etc\; Flags: skipifsourcedoesntexist;Components: {#COMPONENT_MODULE_SKELETON};
;==============================================================================
Source: {#RootPath}\{#MODULE_NAME}\functions\*.m; DestDir: {app}\{#MODULE_NAME}\functions\; Flags: skipifsourcedoesntexist;Components: {#COMPONENT_MODULE_SKELETON};
;==============================================================================
Source: {#RootPath}\{#MODULE_NAME}\help\en_US\images\*.png; DestDir: {app}\{#MODULE_NAME}\help\en_US\images\; Flags: skipifsourcedoesntexist;Components: {#COMPONENT_MODULE_SKELETON};
Source: {#RootPath}\{#MODULE_NAME}\help\en_US\xml\*.xml; DestDir: {app}\{#MODULE_NAME}\help\en_US\xml\; Flags: skipifsourcedoesntexist;Components: {#COMPONENT_MODULE_SKELETON};
;==============================================================================
Source: {#RootPath}\{#MODULE_NAME}\src\*.*; DestDir: {app}\{#MODULE_NAME}\src\; Flags: recursesubdirs skipifsourcedoesntexist;Components: {#COMPONENT_MODULE_SKELETON};
Source: {#RootPath}\{#MODULE_NAME}\builtin\*.*; DestDir: {app}\{#MODULE_NAME}\builtin\; Flags: recursesubdirs skipifsourcedoesntexist;Components: {#COMPONENT_MODULE_SKELETON};
;==============================================================================
Source: {#RootPath}\{#MODULE_NAME}\tests\*.m; DestDir: {app}\{#MODULE_NAME}\tests\; Flags: recursesubdirs skipifsourcedoesntexist;Components: {#COMPONENT_MODULE_SKELETON};
;==============================================================================
