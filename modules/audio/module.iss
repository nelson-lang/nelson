;==============================================================================
; Copyright (c) 2016-present Allan CORNET (Nelson)
;==============================================================================
; This file is part of Nelson.
;==============================================================================
; LICENCE_BLOCK_BEGIN
; SPDX-License-Identifier: LGPL-3.0-or-later
; LICENCE_BLOCK_END
;==============================================================================
#define MODULE_NAME "audio"
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\sndfile.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_AUDIO};
Source: {#RootPath}bin\{#BinPath}\FLAC.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_AUDIO};
Source: {#RootPath}bin\{#BinPath}\libmp3lame.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_AUDIO};
Source: {#RootPath}bin\{#BinPath}\mpg123.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_AUDIO};
Source: {#RootPath}bin\{#BinPath}\ogg.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_AUDIO};
Source: {#RootPath}bin\{#BinPath}\opus.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_AUDIO};
Source: {#RootPath}bin\{#BinPath}\out123.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_AUDIO};
Source: {#RootPath}bin\{#BinPath}\syn123.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_AUDIO};
Source: {#RootPath}bin\{#BinPath}\vorbis.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_AUDIO};
Source: {#RootPath}bin\{#BinPath}\vorbisenc.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_AUDIO};
Source: {#RootPath}bin\{#BinPath}\vorbisfile.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_AUDIO};
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\portaudio.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_AUDIO};
Source: {#RootPath}bin\{#BinPath}\tag.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_AUDIO};
;==============================================================================
Source: {#RootPath}bin\{#BinPath}\libnlsAudio.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_AUDIO};
Source: {#RootPath}bin\{#BinPath}\libnlsAudio_builtin.dll; DestDir: {app}\bin\{#BinPath}\;Components: {#COMPONENT_AUDIO};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\loader.m; DestDir: {app}\modules\{#MODULE_NAME}\;Components: {#COMPONENT_AUDIO};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\startup.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_AUDIO};
Source: {#RootPath}modules\{#MODULE_NAME}\etc\finish.m; DestDir: {app}\modules\{#MODULE_NAME}\etc\;Components: {#COMPONENT_AUDIO};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\functions\*.m; DestDir: {app}\modules\{#MODULE_NAME}\functions\;Components: {#COMPONENT_AUDIO};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*.wav; DestDir: {app}\modules\{#MODULE_NAME}\examples\;Components: {#COMPONENT_AUDIO};
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*.flac; DestDir: {app}\modules\{#MODULE_NAME}\examples\;Components: {#COMPONENT_AUDIO};
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*.ogg; DestDir: {app}\modules\{#MODULE_NAME}\examples\;Components: {#COMPONENT_AUDIO};
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*.mp3; DestDir: {app}\modules\{#MODULE_NAME}\examples\;Components: {#COMPONENT_AUDIO};
Source: {#RootPath}modules\{#MODULE_NAME}\examples\*.m; DestDir: {app}\modules\{#MODULE_NAME}\examples\;Components: {#COMPONENT_AUDIO};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\help\*.qch; DestDir: {app}\modules\{#MODULE_NAME}\help\; Flags: recursesubdirs;Components: {#COMPONENT_AUDIO} and {#COMPONENT_HELP_BROWSER} and {#COMPONENT_HELP_FILES};
;==============================================================================
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.m; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_AUDIO} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.json; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_AUDIO} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.wav; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_AUDIO} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.ogg; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_AUDIO} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
Source: {#RootPath}modules\{#MODULE_NAME}\tests\*.flac; DestDir: {app}\modules\{#MODULE_NAME}\tests\; Flags: recursesubdirs;Components: {#COMPONENT_AUDIO} and {#COMPONENT_TESTS_MANAGER} and {#COMPONENT_UNIT_TESTS};
;==============================================================================
