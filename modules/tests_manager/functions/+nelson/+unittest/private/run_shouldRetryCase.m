%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tf = run_shouldRetryCase(testCase, retryOnlyOn)
  outcome = run_normalizeStatus(testCase);
  policies = run_retryPolicies(retryOnlyOn);
  tf = any(strcmp(policies, 'all')) || any(strcmp(policies, outcome));
end
