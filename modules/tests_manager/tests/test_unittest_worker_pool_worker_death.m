%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
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
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
base = [[tempdir(), ''], 'test_unittest_worker_pool_worker_death_', createGUID()];
files = {
  [base, '_test_before.m']
  [base, '_test_exit_worker.m']
  [base, '_test_after.m']
};
cleanupCommand = '';
for k = 1:length(files)
  cleanupCommand = [cleanupCommand, removeCommand(files{k}), newline];
end
cleanupFiles = onCleanup(@() evalc(cleanupCommand)); %#ok<NLS0004>

filewrite(files{1}, {'asserts.istrue(true);'});
filewrite(files{2}, {'exit(7, ''force'');'});
filewrite(files{3}, {'asserts.istrue(true);'});

privateDir = [nelsonroot(), '/modules/tests_manager/functions/+nelson/+unittest/private'];
previousPath = path();
cleanupPath = onCleanup(@() path(previousPath)); %#ok<NLS0004>
addpath(privateDir);

runtimeOptions = struct('Timeout', [], 'LauncherGrace', 0, ...
  'WorkerPoolInline', false);
testCases = [];
for k = 1:length(files)
  testCase = create_test_case(files{k}, 'Nelson.worker_death', runtimeOptions);
  testCase.worker_pool_eligible = true;
  testCases = [testCases; testCase];
end

results = process_test_cases_pooled_raw(testCases, 1, runtimeOptions);
asserts.isequal(length(results), 3);
asserts.isequal(results(1).status, 'Pass');
asserts.isequal(results(2).status, 'Fail');
asserts.isequal(results(3).status, 'Pass');
asserts.isfalse(results(1).worker_pool_fallback);
asserts.istrue(results(2).worker_pool_fallback);
asserts.istrue(results(3).worker_pool_fallback);
asserts.isfalse(results(1).pool_missing_result);
asserts.isfalse(results(2).pool_missing_result);
asserts.isfalse(results(3).pool_missing_result);
asserts.isfalse(results(1).worker_pool_failure.missing_result);
asserts.istrue(results(2).worker_pool_failure.missing_result);
asserts.istrue(results(3).worker_pool_failure.missing_result);
asserts.isequal(results(2).worker_pool_failure.exit_code, 7);
asserts.isequal(results(3).worker_pool_failure.exit_code, 7);
diagnostics = run_nativeDiagnostics(results(2));
asserts.istrue(diagnostics.worker_pool.used);
asserts.istrue(diagnostics.worker_pool.fallback);
asserts.isfalse(diagnostics.worker_pool.missing_result);
asserts.isequal(diagnostics.worker_pool.failure.exit_code, 7);
for k = 1:length(files)
  run_removeFile(files{k});
end
%=============================================================================
