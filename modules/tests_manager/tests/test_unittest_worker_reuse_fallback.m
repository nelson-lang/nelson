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
skip_if_no_native_unittest_runner();
%=============================================================================
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
base = [[tempdir(), ''], 'test_unittest_worker_reuse_fallback_', createGUID()];
testFile = [base, '_test_ok.m'];
cleanupFile = onCleanup(@() evalc(removeCommand(testFile))); %#ok<NLS0004>
filewrite(testFile, {'asserts.istrue(true);'});

privateDir = [nelsonroot(), '/modules/tests_manager/functions/+nelson/+unittest/private'];
addpath(privateDir);
testCase = create_test_case(testFile, 'Nelson.fallback', struct('Timeout', [], ...
  'LauncherGrace', 0));
nativeResult = struct('exit_code', 1, 'timed_out', false, 'duration', 0, ...
  'stdout', '', 'stderr', '');
missing = worker_pool_collect_worker(testCase, nativeResult);
asserts.istrue(missing.pool_missing_result);
retried = worker_pool_retry_missing(missing, 1);
asserts.isfalse(retried.pool_missing_result);
asserts.isequal(retried.status, 'Pass');
run_removeFile(testFile);
%=============================================================================
