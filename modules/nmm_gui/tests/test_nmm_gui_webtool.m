%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The package manager registers itself as the 'nmm' web tool at startup: the
% web desktop panel sends the same requests as the package manager window.
%=============================================================================
% <--GUI MODE-->
%=============================================================================
skip_testsuite(~strcmp(getnelsonmode(), 'GUI'), _('web tool tests require GUI mode.'));
%=============================================================================
asserts.istrue(__webview__('isWebTool', 'nmm'));
%=============================================================================
response = jsondecode(__webview__('dispatchWebTool', 'nmm', 'i18n', '{}'));
asserts.istrue(isfield(response, 'result'));
asserts.istrue(response.result.ok);
asserts.istrue(ischar(response.result.locale));
asserts.istrue(ischar(response.result.en_US));
%=============================================================================
response = jsondecode(__webview__('dispatchWebTool', 'nmm', 'prefs', '{}'));
asserts.istrue(isfield(response, 'result'));
asserts.istrue(isstruct(response.result));
%=============================================================================
% Read-only requests (list, snapshot, ...) are answered by the nmm_gui_rpc
% facade on the interpreter thread: they are exercised by test_nmm_gui.m and
% cannot be dispatched from a script running on that same thread.
%=============================================================================
% Invalid requests are refused with an error, never executed.
response = jsondecode(__webview__('dispatchWebTool', 'nmm', 'install', '{"name":"bad name;quit"}'));
asserts.istrue(isfield(response, 'error'));
asserts.istrue(contains(response.error.message, 'invalid package name'));
response = jsondecode(__webview__('dispatchWebTool', 'nmm', 'openExternal', '{"url":"file:///etc/passwd"}'));
asserts.istrue(isfield(response, 'error'));
%=============================================================================
