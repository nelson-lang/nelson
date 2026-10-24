%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_cases = annotate_worker_pool_eligibility(test_cases)
  for k = 1:length(test_cases)
    test_cases(k).worker_pool_eligible = compute_worker_pool_eligibility(test_cases(k));
  end
end
%=============================================================================
