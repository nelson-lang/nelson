%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function weights = smooth3KernelWeights(method, windowSize, sd)
  weights = cell(1, 3);
  for dim = 1:3
    radius = (windowSize(dim) - 1) / 2;
    offsets = -radius:radius;
    weights{dim} = smooth3KernelWeightsForDimension(method, offsets, sd);
  end
end
%=============================================================================
function weights = smooth3KernelWeightsForDimension(method, offsets, sd)
  if strcmp(method, 'gaussian')
    weights = exp(-(offsets .^ 2) / (2 * sd * sd));
  else
    weights = ones(1, numel(offsets));
  end
  weights = weights / sum(weights);
end
%=============================================================================
