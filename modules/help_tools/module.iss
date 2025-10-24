;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "help_tools"
;==============================================================================
; xml libraries
Source: {#RootPath}bin\{#BinPath}\iconv-2.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}bin\{#BinPath}\charset-1.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}bin\{#BinPath}\libxml2.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}bin\{#BinPath}\liblzma.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}bin\{#BinPath}\libxslt.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_HELP_TOOLS};
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsHelp_tools.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}bin\{#BinPath}\libnlsHelp_tools_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_HELP_TOOLS};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_HELP_TOOLS};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\resources\banner_homepage.png; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\resources\nelson_help.xsd; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\resources\highlight.pack.js; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\resources\highlight.css; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\resources\nelson_common.css; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\resources\nelson_help.js; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\resources\tex-mml-chtml.js; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\resources\nelson_html.xslt; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\resources\nelson_markdown.xslt; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\resources\nelson_json.xslt; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\resources\nelson_summary2html.xslt; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\resources\nelson_toc2html.xslt; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\resources\nelson_summary2md.xslt; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\resources\nelson_toc2md.xslt; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\resources\help_viewer.html; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\resources\search_results.html; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_HELP_TOOLS};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_HELP_TOOLS};
Source: {#RootPath}modules\{#MODULE_NAME}\functions\private\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\private\;Components: {#COMPONENT_HELP_TOOLS};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.nhz; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_HELP_TOOLS} and {#COMPONENT_HELP_FILES};
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.json; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_HELP_TOOLS} and {#COMPONENT_HELP_FILES};  
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_HELP_TOOLS} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\xml\*.xml; DestDir: {app}\modules\{#MODULE_NAME}\tests\xml; Flags: recursesubdirs;Components: {#COMPONENT_HELP_TOOLS} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\md\*.md; DestDir: {app}\modules\{#MODULE_NAME}\tests\md; Flags: recursesubdirs;Components: {#COMPONENT_HELP_TOOLS} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
