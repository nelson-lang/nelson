%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Dynamic gateway files are not available in the static portable runtime.');
[gatewayName, builtins] = gatewayinfo(modulepath('time', 'builtin'));
%=============================================================================
asserts.isequal(gatewayName, 'time');
asserts.istrue(iscellstr(builtins));
asserts.istrue(any(strcmp(builtins, 'now')));
%=============================================================================
[gatewayName, builtins, state] = gatewayinfo(modulepath('modules_manager', 'builtin'));
asserts.isequal(gatewayName, 'modules_manager');
asserts.istrue(any(strcmp(builtins, 'gatewayinfo')));
asserts.isequal(state, 'loaded');
%=============================================================================
[gatewayName, builtins, state] = gatewayinfo(modulepath('time', 'builtin'));
asserts.isequal(gatewayName, 'time');
asserts.istrue(any(strcmp(state, {'lazy', 'loaded'})));
%=============================================================================
asserts.checkerror('gatewayinfo(1);', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string'));
%=============================================================================
