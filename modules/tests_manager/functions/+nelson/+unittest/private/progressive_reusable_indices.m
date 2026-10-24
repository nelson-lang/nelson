%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function selected = progressive_reusable_indices(testCases, requested, attempt)
  if attempt ~= 1
    selected = [];
    return
  end
  reusable = false(1, length(requested));
  for k = 1:length(requested)
    idx = requested(k);
    reusable(k) = ~testCases(idx).skip && can_pool_test_case(testCases(idx));
  end
  selected = requested(reusable);
end
%=============================================================================
