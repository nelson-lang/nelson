%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function test_suite = run_processPlanCases(...
    planCases, versionText, moduleName, nbWorkers, runtimeOptions, stoponfail)
  test_suite = create_test_suite();
  test_suite.name = moduleName;
  [parallelTests, sequentialTests, benchmarkTests] = run_runtimeCasesFromPlan(... %#ok<NLS0004>
    planCases, versionText, moduleName, runtimeOptions);
  executionBegin = time();
  testCases = process_partitioned_test_cases(...
    parallelTests, sequentialTests, benchmarkTests, ...
    nbWorkers, runtimeOptions, stoponfail); %#ok<NLS0004>
  executionTime = time() - executionBegin;
  test_suite = fill_test_suite_from_cases(test_suite, testCases, executionTime);
end
%=============================================================================
