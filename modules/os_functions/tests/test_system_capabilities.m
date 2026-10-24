%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if iswasm()
  message = 'Native process execution is not available in WebAssembly.';
  identifier = 'Nelson:system:unsupportedPlatform';
  asserts.checkerror('system(''echo single'')', message, identifier);
  asserts.isequal(6 * 7, 42);
  asserts.checkerror('system(["echo first", "echo second"])', message, identifier);
  asserts.isequal(sum([1, 2, 3]), 6);
  asserts.checkerror('system({''echo first'', ''echo second''})', message, identifier);
  asserts.checkerror('unix(''echo unix'')', message, identifier);
  asserts.checkerror('dos(''echo dos'')', message, identifier);
  asserts.isequal(eye(2) * [3; 4], [3; 4]);
else
  [status, output] = system('echo nelson_system_capabilities');
  asserts.isequal(status, 0);
  asserts.istrue(contains(output, 'nelson_system_capabilities'));
end
%=============================================================================
