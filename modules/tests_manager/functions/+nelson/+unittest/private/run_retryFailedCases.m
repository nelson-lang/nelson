%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [raw, status] = run_retryFailedCases(raw, plan, options, status)
  status = logical(status);
  if options.Retry <= 0
    return
  end
  for attempt = 1:options.Retry
    failedCases = run_retryCases(raw, plan, options.RetryOnlyOn);
    if isempty(failedCases)
      status = true;
      return
    end
    retryPlan = plan;
    retryPlan.cases = failedCases;
    [retryStatus, retryRaw] = run_runPlan(retryPlan, options); %#ok<NLS0004>
    retryRaw = run_annotateAttempts(retryRaw, attempt + 1);
    raw = run_mergeRawResults(raw, retryRaw);
    status = retryStatus || run_countUnsuccessful(raw) == 0;
  end
end
