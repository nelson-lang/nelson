%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function grace = run_launcherGrace(options)
  grace = 0;
  if options.Retry <= 0
    return
  end
  policies = run_retryPolicies(options.RetryOnlyOn);
  if any(strcmp(policies, 'timeout')) || any(strcmp(policies, 'all'))
    grace = 90;
  end
end
%=============================================================================
