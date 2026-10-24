%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
if ~ispc()
  skip_testsuite('This test requires Windows long-path support.');
end
enabled = 0;
try
  enabled = winqueryreg('HKEY_LOCAL_MACHINE', 'SYSTEM\CurrentControlSet\Control\FileSystem', 'LongPathsEnabled');
catch
end
if enabled ~= 1
  skip_testsuite('The host long-path policy is not enabled.');
end
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
old = pwd();
restore = onCleanup(@() cd(old));
deep = fullfile(directory, ['unicode ', char(233)]);
while length(deep) < 300
  deep = fullfile(deep, 'nested-directory');
end
mkdir(deep);
expected = strrep(deep, char(92), '/');
asserts.isequal(fullpath(deep), expected);
asserts.isequal(fullpath(fullfile(deep, 'missing', 'file.txt')), [expected, '/missing/file.txt']);
filename = fullfile(deep, 'data.txt');
filewrite(filename, 'long path');
asserts.isequal(fullpath(filename), [expected, '/data.txt']);
asserts.isequal(fileread(filename), 'long path');
cd(deep);
asserts.isequal(pwd(), expected);
asserts.isequal(fullpath('./data.txt'), [expected, '/data.txt']);
asserts.isequal(fullpath({'./data.txt'; './missing.txt'}), {[expected, '/data.txt']; [expected, '/missing.txt']});
clear restore cleanup;
