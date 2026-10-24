%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ispc()
  skip_testsuite('The native performance harness requires Windows.');
end
script = fullfile(nelsonroot(), 'modules', 'main', 'tests', 'test_nelsonc_performance_control.ps1');
powershell = fullfile(getenv('WINDIR'), 'System32', 'WindowsPowerShell', 'v1.0', 'powershell.exe');
[status, output] = system([powershell, ' -NoProfile -NonInteractive -File "', script, '"']);
asserts.isequal(status, 0);
asserts.istrue(contains(output, 'PASS interleaved performance control'));
