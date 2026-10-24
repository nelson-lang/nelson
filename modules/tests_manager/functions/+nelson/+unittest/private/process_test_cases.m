%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_cases_updated = process_test_cases(...
    test_cases, nbWorkers, nbTotalTests, initialIndex, stoponfail, runtimeOptions)
  if isempty(test_cases)
    test_cases_updated = [];
    return
  end
  annotatedCases = annotate_worker_pool_eligibility(test_cases);
  if use_inline_unittest_runner()
    abortedCode = launcher_aborted_code();
    fmtStart = test_start_format(nbTotalTests);
    test_cases_updated = process_test_cases_batched(...
      annotatedCases, 1, abortedCode, initialIndex, nbTotalTests, fmtStart, stoponfail);
    return
  end
  test_cases_updated = process_test_cases_progressive(...
    annotatedCases, nbWorkers, initialIndex, nbTotalTests, stoponfail);
end
%=============================================================================
