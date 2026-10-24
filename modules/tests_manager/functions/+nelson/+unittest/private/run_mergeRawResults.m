%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function raw = run_mergeRawResults(raw, retryRaw)
  retry = run_resultCases(retryRaw);
  if isempty(retry)
    return
  end
  for s = 1:length(raw.tests_list)
    for c = 1:length(raw.tests_list(s).test_cases_list)
      current = raw.tests_list(s).test_cases_list(c);
      idx = run_findRetryCase(retry, current.name);
      if idx > 0
        raw.tests_list(s).test_cases_list(c) = retry(idx);
      end
    end
  end
  raw = run_recomputeRawSummary(raw);
end
