%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function raw = run_annotateAttempts(raw, attempts)
  if ~isfield(raw, 'tests_list')
    return
  end
  for s = 1:length(raw.tests_list)
    cases = raw.tests_list(s).test_cases_list;
    idx = 1;
    while idx <= length(cases)
      cases(idx).attempts = attempts;
      idx = idx + 1;
    end
    raw.tests_list(s).test_cases_list = cases;
  end
end
