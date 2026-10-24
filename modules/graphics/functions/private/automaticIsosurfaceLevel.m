%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = automaticIsosurfaceLevel(V)
  finiteValues = V(isfinite(V));
  if isempty(finiteValues)
    value = [];
    return
  end
  minValue = min(finiteValues(:));
  maxValue = max(finiteValues(:));
  if minValue == maxValue
    value = [];
    return
  end
  edges = automaticIsosurfaceEdges(finiteValues, minValue, maxValue);
  counts = automaticIsosurfaceCounts(finiteValues, edges);
  value = medianHistogramLevel(edges, counts);
end
%=============================================================================
function edges = automaticIsosurfaceEdges(values, minValue, maxValue)
  binCount = min(64, max(8, ceil(sqrt(numel(values)))));
  edges = linspace(minValue, maxValue, binCount + 1);
end
%=============================================================================
function counts = automaticIsosurfaceCounts(values, edges)
  binCount = numel(edges) - 1;
  counts = zeros(1, binCount);
  for k = 1:binCount
    if k == binCount
      counts(k) = sum(values >= edges(k) & values <= edges(k + 1));
    else
      counts(k) = sum(values >= edges(k) & values < edges(k + 1));
    end
  end
end
%=============================================================================
function value = medianHistogramLevel(edges, counts)
  target = sum(counts) / 2;
  cumulative = cumsum(counts);
  idx = find(cumulative >= target, 1);
  value = (edges(idx) + edges(idx + 1)) / 2;
end
%=============================================================================
