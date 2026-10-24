%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function names = run_retryNames(raw, retryOnlyOn)
  names = {};
  cases = run_resultCases(raw);
  for k = 1:length(cases)
    if run_shouldRetryCase(cases(k), retryOnlyOn)
      names = [names; cases(k).name];
    end
  end
end
