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
  'Dynamic gateway loading is not available in the static portable runtime.');
asserts.isequal(nargin('addgateway'), -3);
asserts.isequal(nargout('addgateway'), 0);
%=============================================================================
asserts.checkerror('addgateway(1);', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string'));
asserts.checkerror('addgateway(modulepath(''time'', ''builtin''), 1);', sprintf(_('Wrong type for argument #%d: %s expected.'), 2, 'string'));
asserts.checkerror('addgateway(modulepath(''time'', ''builtin''), ''time'', 1);', sprintf(_('Wrong type for argument #%d: %s expected.'), 3, 'string'));
asserts.checkerror('addgateway(modulepath(''time'', ''builtin''), ''time'', ''bad'');', sprintf(_('%s expected.'), 'Wrong value for argument #3: ''auto'' or ''loaded'''));
%=============================================================================
gateway_path = modulepath('fftw', 'builtin');
removegateway(gateway_path);
addgateway(gateway_path, 'auto');
[~, ~, state] = gatewayinfo(gateway_path);
asserts.istrue(any(strcmp(state, {'lazy', 'loaded'})));
removegateway(gateway_path);
addgateway(gateway_path, 'loaded');
[~, ~, state] = gatewayinfo(gateway_path);
asserts.isequal(state, 'loaded');
removegateway(gateway_path);
addgateway(gateway_path, 'auto');
%=============================================================================
previous_force_loaded = getenv('NELSON_GATEWAY_FORCE_LOADED');
cleanup_force_loaded = onCleanup(@() setenv('NELSON_GATEWAY_FORCE_LOADED', previous_force_loaded));
setenv('NELSON_GATEWAY_FORCE_LOADED', '1');
removegateway(gateway_path);
addgateway(gateway_path, 'auto');
[~, ~, state] = gatewayinfo(gateway_path);
asserts.isequal(state, 'loaded');
clear cleanup_force_loaded
setenv('NELSON_GATEWAY_FORCE_LOADED', previous_force_loaded);
removegateway(gateway_path);
addgateway(gateway_path, 'auto');
%=============================================================================
