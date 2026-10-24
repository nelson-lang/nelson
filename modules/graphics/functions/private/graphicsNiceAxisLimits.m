%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Expected automatic axis limits. The data extent [lo, hi] is snapped outward
% onto a {1,2,5}x10^k grid. Among the steps whose outward-snapped limits span at
% most maxIntervals intervals, the one adding the least padding beyond the data
% wins; ties resolve to the finest step. This is the same rule the axis
% auto-tick engine (GOAxisHelpers niceStep) uses, so a frozen pair of limits and
% its ticks match what an automatic axis would settle on for the same range.
function [limits, ticks] = graphicsNiceAxisLimits(lo, hi, maxIntervals)
  if nargin < 3
    maxIntervals = 10;
  end
  step = niceAxisStep(lo, hi, maxIntervals);
  a = floor(lo / step) * step;
  b = ceil(hi / step) * step;
  n = round((b - a) / step);
  ticks = a + (0:n) * step;
  ticks(abs(ticks) < step * 1e-9) = 0;
  limits = [a, b];
end
%=============================================================================
function step = niceAxisStep(amin, amax, maxIntervals)
  range = abs(amax - amin);
  if ~isfinite(range) || range <= 0
    step = 1;
    return
  end
  if maxIntervals < 1
    maxIntervals = 1;
  end
  rawStep = range / maxIntervals;
  loDecade = floor(log10(rawStep)) - 1;
  hiDecade = ceil(log10(range)) + 1;
  multipliers = [1 2 5];
  bestStep = 0;
  bestPadding = inf;
  tol = 1e-9 * max(1, range);
  for decade = loDecade:hiDecade
    scale = 10 ^ decade;
    for m = multipliers
      s = m * scale;
      a = floor(amin / s) * s;
      b = ceil(amax / s) * s;
      intervals = round((b - a) / s);
      if intervals < 1 || intervals > maxIntervals
        continue
      end
      padding = (amin - a) + (b - amax);
      if padding < bestPadding - tol
        bestPadding = padding;
        bestStep = s;
      end
    end
  end
  if bestStep <= 0
    step = range / maxIntervals;
  else
    step = bestStep;
  end
end
%=============================================================================
