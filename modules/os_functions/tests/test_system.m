%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(iswasm(), 'Requires a native shell and process creation, unavailable in WebAssembly.');
asserts.isequal(nargin('system'), 1);
asserts.isequal(nargout('system'), 2);
%=============================================================================
[status, output, duration] = system('echo nelson_system_test');
%=============================================================================
asserts.isequal(status, 0);
asserts.istrue(contains(output, 'nelson_system_test'));
asserts.istrue(isnumeric(duration));
%=============================================================================
if ispc()
  detachedCommand = 'cmd.exe /c exit 0 &';
else
  detachedCommand = 'true &';
end
[status, output, duration] = system(detachedCommand);
asserts.isequal(status, 0);
asserts.istrue(ischar(output));
asserts.istrue(isnumeric(duration));
%=============================================================================
if ispc()
  [status, output, duration] = system('start /b cmd.exe /c exit 0');
  asserts.isequal(status, 0);
  asserts.istrue(ischar(output));
  asserts.istrue(isnumeric(duration));
end
%=============================================================================
