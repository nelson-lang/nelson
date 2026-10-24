%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [poolable, direct] = partition_poolable_test_cases(test_cases)
  mask = worker_pool_eligible_mask(test_cases);
  poolable = test_cases(mask);
  direct = test_cases(~mask);
  poolable = poolable(:);
  direct = direct(:);
end
%=============================================================================
