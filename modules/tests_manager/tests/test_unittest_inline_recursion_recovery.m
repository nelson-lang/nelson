%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
savedPath = path();
savedRecursionDepth = max_recursion_depth();
privateDir = [modulepath('tests_manager'), '/functions/+nelson/+unittest/private'];
addpath(privateDir);
testDirectory = [tempdir(), 'test_unittest_inline_recursion_', createGUID()];
mkdir(testDirectory);
%#ok<NLS0004>
cleanupFiles = onCleanup(@() evalc([...
  'path(''', savedPath, ''');', ...
  'if isdir(''', testDirectory, '''), rmdir(''', testDirectory, ''', ''s''), end']));

failingFile = [testDirectory, '/test_lower_recursion.m'];
followingFile = [testDirectory, '/test_after_recursion.m'];
filewrite(failingFile, {
  'callerSentinel = ''fixture workspace'';'
  'fixtureOnlyVariable = 123;'
  'max_recursion_depth(1);'
  'error(''Nelson:test:recursionFixture'', ''Intentional failure after lowering recursion depth.'');'
  });
filewrite(followingFile, {
  ['asserts.isequal(max_recursion_depth(), ', int2str(savedRecursionDepth), ');']
  'asserts.isequal(exist(''fixtureOnlyVariable'', ''var''), 0);'
  'asserts.isequal(exist(''callerSentinel'', ''var''), 0);'
  });

runtimeOptions = struct('Timeout', [], 'LauncherGrace', 0);
cases = [initialize_test_case(failingFile, '', runtimeOptions); ...
  initialize_test_case(followingFile, '', runtimeOptions)];
cases(1).command = 'inline';
cases(2).command = 'inline';
callerSentinel = 'caller workspace';
try
  [status, message, results] = run_accumulator_inline(cases);
  restoredRecursionDepth = max_recursion_depth();
catch exception
  % Recover here as well so a failing regression cannot poison its own runner.
  max_recursion_depth(savedRecursionDepth);
  rethrow(exception);
end
max_recursion_depth(savedRecursionDepth);

asserts.isequal(restoredRecursionDepth, savedRecursionDepth);
asserts.isequal(length(results), 2);
asserts.isfalse(status(1) == 0);
asserts.isfalse(isempty(results(1).runfile_result.msg));
asserts.isequal(status(2), 0);
asserts.isequal(results(2).runfile_result.r, 0);
asserts.isequal(callerSentinel, 'caller workspace');
asserts.isequal(exist('fixtureOnlyVariable', 'var'), 0);
%=============================================================================
