%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = smooth3ApplyWeights(data, weights, dim)
  out = zeros(size(data));
  radius = (numel(weights) - 1) / 2;
  n = size(data, dim);
  base = 1:n;
  idx = repmat({':'}, 1, ndims(data));
  for k = 1:numel(weights)
    src = min(max(base + k - radius - 1, 1), n);
    idx{dim} = src;
    out = out + weights(k) * data(idx{:});
  end
end
%=============================================================================
