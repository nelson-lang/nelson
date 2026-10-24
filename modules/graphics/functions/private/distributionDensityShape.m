%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [centers, widths] = distributionDensityShape(values, maxWidth, numBins)
  values = double(values(:));
  values = values(isfinite(values));
  if isempty(values)
    centers = [];
    widths = [];
    return
  end
  if nargin < 3
    numBins = 20;
  end
  if min(values) == max(values)
    centers = values(1);
    widths = maxWidth;
    return
  end
  edges = linspace(min(values), max(values), numBins + 1);
  counts = zeros(1, numBins);
  for k = 1:numel(values)
    idx = find(edges <= values(k), 1, 'last');
    if idx >= numel(edges)
      idx = numBins;
    end
    counts(idx) = counts(idx) + 1;
  end
  counts = smoothCounts(counts);
  centers = (edges(1:end - 1) + edges(2:end)) / 2;
  if max(counts) == 0
    widths = zeros(size(counts));
  else
    widths = maxWidth * counts / max(counts);
  end
end
%=============================================================================
function y = smoothCounts(x)
  y = x;
  if numel(x) < 3
    return
  end
  for k = 2:numel(x) - 1
    y(k) = (x(k - 1) + 2 * x(k) + x(k + 1)) / 4;
  end
end
%=============================================================================
