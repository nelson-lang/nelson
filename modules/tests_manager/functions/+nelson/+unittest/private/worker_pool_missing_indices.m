%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function indices = worker_pool_missing_indices(test_cases)
  indices = [];
  for k = 1:length(test_cases)
    if isfield(test_cases(k), 'pool_missing_result') && test_cases(k).pool_missing_result
      indices = [indices, k];
    end
  end
end
%=============================================================================
