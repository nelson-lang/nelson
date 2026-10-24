%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function chunks = worker_pool_chunks(test_cases, nbWorkers)
  nbChunks = min(nbWorkers, length(test_cases));
  chunks = cell(1, nbChunks);
  weights = zeros(1, nbChunks);
  assigned = zeros(1, length(test_cases));
  for k = 1:length(test_cases)
    [~, idx] = min(weights);
    test_cases(k).pool_index = k;
    test_cases(k).worker_pool_used = true;
    assigned(k) = idx;
    weights(idx) = weights(idx) + test_case_weight(test_cases(k));
  end
  for k = 1:nbChunks
    chunk = test_cases(assigned == k);
    chunks{k} = chunk(:);
  end
end
%=============================================================================
