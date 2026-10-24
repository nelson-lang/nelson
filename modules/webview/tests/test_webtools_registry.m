%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Web tools registry: a module registers the native dispatcher of its HTML
% application; the web desktop forwards page requests to it.
%=============================================================================
% <--GUI MODE-->
%=============================================================================
skip_testsuite(~strcmp(getnelsonmode(), 'GUI'), _('web tools registry tests require GUI mode.'));
%=============================================================================
asserts.isfalse(__webview__('isWebTool', 'no_such_tool'));
response = jsondecode(__webview__('dispatchWebTool', 'no_such_tool', 'ping', '{}'));
asserts.istrue(isfield(response, 'error'));
asserts.istrue(contains(response.error.message, 'unknown web tool'));
%=============================================================================
% A library that cannot be loaded is reported when the tool is used, not when
% it is registered (modules register from startup.m before any window).
missingLibrary = [tempdir(), 'libnlsMissingWebTool', getdynlibext()];
__webview__('registerWebTool', 'missing_tool', missingLibrary, 'nlsMissingDispatch');
asserts.istrue(__webview__('isWebTool', 'missing_tool'));
response = jsondecode(__webview__('dispatchWebTool', 'missing_tool', 'ping', '{}'));
asserts.istrue(isfield(response, 'error'));
asserts.istrue(contains(response.error.message, 'cannot load'));
asserts.istrue(__webview__('unregisterWebTool', 'missing_tool'));
asserts.isfalse(__webview__('unregisterWebTool', 'missing_tool'));
asserts.isfalse(__webview__('isWebTool', 'missing_tool'));
%=============================================================================
