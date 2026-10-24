%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--CLI MODE-->
previousWorkers = maxNumCompThreads(1);
bench_fail_file = [[tempdir(), ''], 'bench_test_run_failure_', createGUID(), '.m'];
bench_result_file = [[tempdir(), ''], 'test_run_bench_failure_', createGUID(), '.json'];
cleanupWorkers = onCleanup(@() maxNumCompThreads(previousWorkers)); %#ok<NLS0004>
cleanupBench = onCleanup(@() run_removeFile(bench_fail_file)); %#ok<NLS0004>
cleanupResult = onCleanup(@() run_removeFile(bench_result_file)); %#ok<NLS0004>
filewrite(bench_fail_file, {'asserts.istrue(false);'});

output = evalc('status = test_run({bench_fail_file}, bench_result_file);');
asserts.isfalse(status);
asserts.istrue(contains(output, 'Fail'));
results = jsondecode(bench_result_file, '-file');
asserts.isequal(results.errors, 1);
asserts.isequal(results.bench, 1);
asserts.isequal(results.tests_list.test_cases_list.status, 'Fail');
%=============================================================================
