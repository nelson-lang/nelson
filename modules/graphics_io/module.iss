;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "graphics_io"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsGraphics_io.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GRAPHICS};
Source: {#RootPath}bin\{#BinPath}\libnlsGraphics_io_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GRAPHICS};
Source: {#RootPath}bin\{#BinPath}\libnlsGraphics_io_qt.dll; DestDir: {app}\bin\{#BinPath}\; Flags: skipifsourcedoesntexist; Components: {#COMPONENT_GRAPHICS} and {#COMPONENT_GUI};
Source: {#RootPath}bin\{#BinPath}\gif.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GRAPHICS};
Source: {#RootPath}bin\{#BinPath}\jpeg62.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GRAPHICS};
Source: {#RootPath}bin\{#BinPath}\libpng16.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GRAPHICS};
Source: {#RootPath}bin\{#BinPath}\tiff.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GRAPHICS};
Source: {#RootPath}bin\{#BinPath}\turbojpeg.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GRAPHICS};
Source: {#RootPath}bin\{#BinPath}\libwebp.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GRAPHICS};
Source: {#RootPath}bin\{#BinPath}\libsharpyuv.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GRAPHICS};
; libtiff/libpng pull in zlib (z.dll) and LZMA (liblzma.dll); those ship
; with several data modules and with XML, but a GRAPHICS-only install needs
; them for figure image export, so ship them with the graphics component too.
Source: {#RootPath}bin\{#BinPath}\z.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GRAPHICS};
Source: {#RootPath}bin\{#BinPath}\liblzma.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_GRAPHICS};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_GRAPHICS};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_GRAPHICS};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_GRAPHICS};
;==============================================================================
;Source: {#RootPath}modules\{#MODULE_NAME}\resources\*.*; DestDir: {app}\modules\{#MODULE_NAME}\resources\;Components: {#COMPONENT_GRAPHICS};
;==============================================================================
;Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_GRAPHICS};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*; DestDir: {app}\modules\{#MODULE_NAME}\examples\; Flags: recursesubdirs;Components: {#COMPONENT_GRAPHICS};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.nhz; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_GUI} and {#COMPONENT_GRAPHICS} and {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_GRAPHICS} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
