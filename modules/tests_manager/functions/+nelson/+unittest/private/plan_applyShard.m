%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function cases = plan_applyShard(cases, shardIndex, shardCount)
  if shardCount <= 1
    return
  end
  keep = false(length(cases), 1);
  for k = 1:length(cases)
    keep(k) = mod(k - 1, shardCount) + 1 == shardIndex;
  end
  cases = cases(keep);
end
%=============================================================================
