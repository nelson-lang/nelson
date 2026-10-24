%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function cases = report_htmlRawCases(results)
  cases = struct([]);
  if ~isfield(results, 'raw') || ~isfield(results.raw, 'tests_list')
    return
  end
  suites = results.raw.tests_list;
  for suite = suites(:)'
    if isfield(suite, 'test_cases_list')
      currentCases = suite.test_cases_list;
      for testCase = currentCases(:)'
        cases = [cases; testCase];
      end
    end
  end
end
%=============================================================================
