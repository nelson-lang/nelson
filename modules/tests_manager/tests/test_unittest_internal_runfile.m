%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(which('test_runfile'), '');
asserts.isequal(which('nelson.unittest.internal.runfile'), '');
asserts.isequal(which('test_run_processes'), '');
asserts.isequal(which('test_parsetags'), '');

testFile = [nelsonroot(), '/modules/tests_manager/tests/test_skip_testsuite_condition_false.m'];
results = nelson.unittest.run(testFile, 'Workers', 1);
asserts.istrue(results.status);
asserts.isequal(length(results.cases), 1);
asserts.isequal(results.cases(1).outcome, 'passed');

testDirectory = [[tempdir(), ''], ...
  'test_unittest_runfile_time_shadow_', createGUID()];
mkdir(testDirectory);
timeShadowFile = [testDirectory, '/test_time_shadow.m'];
%#ok<NLS0004>
cleanupFile = onCleanup(@() evalc([...
  'if isfile(''', timeShadowFile, '''), rmfile(''', timeShadowFile, '''), end;', ...
  'if isdir(''', testDirectory, '''), rmdir(''', testDirectory, '''), end']));
filewrite(timeShadowFile, {
  'time = [0, 1];'
  'assert_isequal(time, [0, 1]);'
  });

result = runfile(timeShadowFile, 0);
asserts.isequal(result.r, 0);
asserts.isequal(result.msg, '');
asserts.isequal(exist('time', 'var'), 0);

result = runfile_inline(timeShadowFile, 0);
asserts.isequal(result.r, 0);
asserts.isequal(result.msg, '');
asserts.isequal(exist('time', 'var'), 0);

result = runfile_nested(timeShadowFile, 0);
asserts.isequal(result.r, 0);
asserts.isequal(result.msg, '');
asserts.isequal(exist('time', 'var'), 0);

result = worker_pool_safe_runfile(timeShadowFile, true);
asserts.isequal(result.r, 0);
asserts.isequal(result.msg, '');
asserts.isequal(exist('time', 'var'), 0);
%=============================================================================
