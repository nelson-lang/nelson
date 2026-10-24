%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function nbPoolable = count_poolable_test_cases(test_cases)
  nbPoolable = sum(double(worker_pool_eligible_mask(test_cases)));
end
%=============================================================================
