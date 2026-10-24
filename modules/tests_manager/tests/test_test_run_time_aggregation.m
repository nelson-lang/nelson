%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--CLI MODE-->
previousWorkers = maxNumCompThreads(1);
cleanupWorkers = onCleanup(@() maxNumCompThreads(previousWorkers)); %#ok<NLS0004>
test_file_1 = [[tempdir(), ''], 'test_run_time_aggregation_1_', createGUID(), '.m'];
test_file_2 = [[tempdir(), ''], 'test_run_time_aggregation_2_', createGUID(), '.m'];
result_file = [[tempdir(), ''], 'test_run_time_aggregation_', createGUID(), '.json'];
cleanupTestFile1 = onCleanup(@() run_removeFile(test_file_1)); %#ok<NLS0004>
cleanupTestFile2 = onCleanup(@() run_removeFile(test_file_2)); %#ok<NLS0004>
cleanupResult = onCleanup(@() run_removeFile(result_file)); %#ok<NLS0004>

filewrite(test_file_1, {'sleep(0.01);'; 'asserts.istrue(true);'});
filewrite(test_file_2, {'sleep(0.01);'; 'asserts.istrue(true);'});

output = evalc('status = test_run({test_file_1, test_file_2}, result_file);');
asserts.istrue(status);
asserts.istrue(contains(output, 'test_run_time_aggregation'));

results = jsondecode(result_file, '-file');
test_cases_time = sum([results.tests_list.test_cases_list.time]);
asserts.istrue(results.time >= max([results.tests_list.test_cases_list.time]));
asserts.istrue(results.time >= test_cases_time);
asserts.istrue(results.time <= results.total_time);

rmfile(test_file_1);
rmfile(test_file_2);
rmfile(result_file);
