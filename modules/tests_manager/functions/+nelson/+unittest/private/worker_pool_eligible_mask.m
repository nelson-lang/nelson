%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function mask = worker_pool_eligible_mask(test_cases)
  mask = false(1, length(test_cases));
  for k = 1:length(test_cases)
    mask(k) = can_pool_test_case(test_cases(k));
  end
end
%=============================================================================
