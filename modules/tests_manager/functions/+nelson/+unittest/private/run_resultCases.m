%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function cases = run_resultCases(results)
  raw = results;
  if isfield(results, 'raw')
    raw = results.raw;
  end
  cases = struct([]);
  if ~isfield(raw, 'tests_list')
    return
  end
  for suite = raw.tests_list(:)'
    if isfield(suite, 'test_cases_list')
      for testCase = suite.test_cases_list(:)'
        cases = [cases; testCase];
      end
    end
  end
end
