%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function cases = report_resultCases(results)
  if isfield(results, 'cases')
    cases = results.cases;
    return
  end
  raw = results;
  if isfield(results, 'raw')
    raw = results.raw;
  end
  cases = struct([]);
  suites = raw.tests_list;
  for suite = suites(:)'
    if isfield(suite, 'test_cases_list')
      currentCases = suite.test_cases_list;
      for testCase = currentCases(:)'
        cases = [cases; testCase];
      end
    end
  end
end
