%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Minimum-variance colormap by median-cut over the unique colors of P.
% P is N-by-3 in [0, 1]. Returns a K-by-3 colormap with K <= n. When the image
% has n or fewer distinct colors the palette is lossless.
function map = ip_median_cut_map(P, n)
  [U, ~, ic] = unique(P, 'rows');
  counts = accumarray(ic, 1);
  M = size(U, 1);
  if M <= n
    map = U;
    return;
  end
  boxes = {(1:M)'};
  while numel(boxes) < n
    bestBox = 0;
    bestExtent = -1;
    bestDim = 1;
    for i = 1:numel(boxes)
      ids = boxes{i};
      if numel(ids) < 2
        continue;
      end
      colors = U(ids, :);
      extent = max(colors, [], 1) - min(colors, [], 1);
      [mx, dim] = max(extent);
      if mx > bestExtent
        bestExtent = mx;
        bestBox = i;
        bestDim = dim;
      end
    end
    if bestBox == 0
      break;
    end
    ids = boxes{bestBox};
    [~, order] = sort(U(ids, bestDim));
    ids = ids(order);
    w = counts(ids);
    total = sum(w);
    splitPos = find(cumsum(w) >= total / 2, 1, 'first');
    if isempty(splitPos) || splitPos >= numel(ids)
      splitPos = numel(ids) - 1;
    end
    if splitPos < 1
      splitPos = 1;
    end
    boxes{bestBox} = ids(1:splitPos);
    boxes{end + 1} = ids(splitPos + 1:end);
  end
  K = numel(boxes);
  map = zeros(K, 3);
  for i = 1:K
    ids = boxes{i};
    w = counts(ids);
    map(i, :) = sum(U(ids, :) .* w, 1) / sum(w);
  end
end
%=============================================================================
