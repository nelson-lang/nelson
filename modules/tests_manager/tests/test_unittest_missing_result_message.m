%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
skip_if_no_native_unittest_runner();
%=============================================================================
% A test process that returns no result must report its stderr, not an empty
% message (the report then printed the whole test case struct).
runtimeOptions = struct('Timeout', [], 'LauncherGrace', 0);
testCase = initialize_test_case([modulepath('tests_manager', 'tests'), '/bug_clearall.m'], '', runtimeOptions);
testCase.res_cmd = 1;
testCase.exit_code = 1;
testCase.msg = 'error text from the process';
testCase.stderr = ['error text from the process', newline];
R = post_run_test_case(testCase, false);
asserts.isequal(R.status, 'Fail');
asserts.isequal(R.msg, 'error text from the process');
%=============================================================================
testCase.res_cmd = -1;
testCase.exit_code = -1;
testCase.msg = '';
testCase.stderr = '';
R = post_run_test_case(testCase, false);
asserts.isequal(R.status, 'Fail');
asserts.isequal(R.msg, 'Test process ended with exit code -1 before returning a result.');
%=============================================================================
% An onCleanup task failing when the test ends must fail with its own message.
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
cleanupFile = [[tempdir(), ''], 'test_unittest_missing_result_message_', createGUID(), '.m'];
cleanupRemove = onCleanup(@() evalc(removeCommand(cleanupFile))); %#ok<NLS0004>
filewrite(cleanupFile, {'% <--ENGLISH IMPOSED-->'; ...
  'c = onCleanup(@() error(''cleanup task failed''));'; 'a = 1;'});
results = nelson.unittest.run({cleanupFile}, 'Workers', 1);
asserts.isequal(results.cases(1).outcome, 'failed');
asserts.istrue(contains(results.cases(1).message, 'cleanup task failed'));
%=============================================================================
