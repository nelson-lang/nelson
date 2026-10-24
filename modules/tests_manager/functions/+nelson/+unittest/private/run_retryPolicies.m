%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function policies = run_retryPolicies(retryOnlyOn)
  if ischar(retryOnlyOn)
    policies = {retryOnlyOn};
  elseif iscellstr(retryOnlyOn)
    policies = retryOnlyOn(:)';
  else
    error('Nelson:tests_manager:invalidRetryOnlyOn', 'RetryOnlyOn must be a string or cell of strings.');
  end
  for k = 1:length(policies)
    if strcmp(policies{k}, 'Fail')
      policies{k} = 'failed';
    end
  end
end
