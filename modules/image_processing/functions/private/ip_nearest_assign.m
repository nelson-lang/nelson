%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Assign each pixel to the nearest colormap entry (squared Euclidean distance).
% P is N-by-3 in [0, 1]; map is K-by-3. Returns N-by-1 zero-based indices.
function idx = ip_nearest_assign(P, map)
  N = size(P, 1);
  K = size(map, 1);
  best = inf(N, 1);
  idx = zeros(N, 1);
  for k = 1:K
    d = (P(:, 1) - map(k, 1)) .^ 2 + (P(:, 2) - map(k, 2)) .^ 2 + (P(:, 3) - map(k, 3)) .^ 2;
    upd = d < best;
    best(upd) = d(upd);
    idx(upd) = k - 1;
  end
end
%=============================================================================
