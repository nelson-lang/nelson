%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Uniform-quantization colormap. Each channel is snapped to one of L grid
% points with L = floor(1/tol) + 1. The map lists the distinct grid colors that
% occur, ordered by their linear grid index. P is N-by-3 in [0, 1].
function map = ip_uniform_map(P, tol)
  L = floor(1 / tol) + 1;
  levels = round(P * (L - 1));
  Q = unique(levels, 'rows');
  linIndex = Q(:, 1) + Q(:, 2) * L + Q(:, 3) * L * L;
  [~, order] = sort(linIndex);
  Q = Q(order, :);
  map = Q / (L - 1);
end
%=============================================================================
