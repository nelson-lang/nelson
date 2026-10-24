%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function test_cases_updated = process_test_cases_batched(...
    test_cases, nbWorkers, abortedCode, indexTest, nbTotalTests, fmtStart, stoponfail)
  accumulator = [];
  test_cases_updated = [];
  nbTests = length(test_cases);
  for testIndex = 1:length(test_cases)
    accumulator = [accumulator; test_cases(testIndex)];
    if length(accumulator) == nbWorkers || testIndex == nbTests
      accumulator = run_test_batch(accumulator, abortedCode, nbWorkers);
      displayArgs = {test_cases_updated, accumulator, indexTest, nbTotalTests, fmtStart};
      [test_cases_updated, indexTest] = display_test_batch(displayArgs{:}); %#ok<NLS0004>
      if stoponfail && has_failed_cases(accumulator)
        break
      end
      accumulator = [];
    end
  end
  test_cases_updated = remove_test_case_runtime_fields(test_cases_updated);
end
%=============================================================================
