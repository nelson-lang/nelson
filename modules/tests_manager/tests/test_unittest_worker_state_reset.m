%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
testDirectory = [[tempdir(), ''], ...
  'test_unittest_worker_state_reset_', createGUID()];
mkdir(testDirectory);
filename = [testDirectory, '/test_worker_state_reset.m'];
%#ok<NLS0004>
cleanupFile = onCleanup(@() evalc([...
  'if isfile(''', filename, '''), rmfile(''', filename, '''), end;', ...
  'if isdir(''', testDirectory, '''), rmdir(''', testDirectory, '''), end']));

savedPath = path();
savedDirectory = pwd();
savedFormat = format();
savedRng = rng();
filewrite(filename, {
  ['addpath(''', testDirectory, ''');']
  ['cd(''', testDirectory, ''');']
  'format long;'
  'rng(42);'
  });

result = worker_pool_safe_runfile(filename, true);
asserts.isequal(result.r, 0);
asserts.isequal(path(), savedPath);
asserts.isequal(pwd(), savedDirectory);
restoredFormat = format();
asserts.isequal(restoredFormat.NumericFormatValue, savedFormat.NumericFormatValue);
asserts.isequal(restoredFormat.LineSpacingValue, savedFormat.LineSpacingValue);
asserts.isequal(...
  restoredFormat.TruncateMatricesValue, savedFormat.TruncateMatricesValue);
asserts.isequal(rng(), savedRng);
%=============================================================================
