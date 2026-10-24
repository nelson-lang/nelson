%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function cases = run_retryCases(raw, plan, retryOnlyOn)
  names = run_retryNames(raw, retryOnlyOn);
  cases = struct([]);
  for k = 1:length(plan.cases)
    if any(strcmp(names, plan.cases(k).name))
      cases = [cases; plan.cases(k)];
    end
  end
end
