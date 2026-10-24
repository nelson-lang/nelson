%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function applyContourAxesState(ax, h)
  limits = [min(h.XData(:)), max(h.XData(:)), min(h.YData(:)), max(h.YData(:))];
  if isempty(limits)
    return
  end
  axis(ax, limits);
  if strcmp(ax.XTickMode, 'auto')
    ticks = contourAxisTicks(limits(1:2), contourAxisTarget(ax, true, limits(1:2)));
    if ~isempty(ticks)
      ax.XTick = ticks;
    end
  end
  if strcmp(ax.YTickMode, 'auto')
    ticks = contourAxisTicks(limits(3:4), contourAxisTarget(ax, false, limits(3:4)));
    if ~isempty(ticks)
      ax.YTick = ticks;
    end
  end
end
%=============================================================================
function ticks = contourAxisTicks(limits, targetIntervals)
  ticks = [];
  span = diff(limits);
  if ~isfinite(span) || span <= 0
    return
  end
  step = contourNiceStep(span / targetIntervals);
  first = ceil(limits(1) / step) * step;
  last = floor(limits(2) / step) * step;
  ticks = first:step:last;
  ticks(abs(ticks) < step * 1e-10) = 0;
end
%=============================================================================
function targetIntervals = contourAxisTarget(ax, horizontalAxis, limits)
  span = diff(limits);
  if horizontalAxis
    targetIntervals = 9;
    if span > 10 && span <= 15
      targetIntervals = 6;
    end
    if ax.Position(3) < 0.5
      if span <= 2.5 || span > 20
        targetIntervals = 4;
      else
        targetIntervals = 3;
      end
    end
    return
  end
  targetIntervals = 9;
  if span > 10 && span <= 15
    targetIntervals = 6;
  end
  if ax.Position(4) < 0.5
    if span <= 2.5 || span > 20
      targetIntervals = 10;
    else
      targetIntervals = 6;
    end
  end
end
%=============================================================================
function step = contourNiceStep(value)
  if ~isfinite(value) || value <= 0
    step = 1;
    return
  end
  scale = 10 ^ floor(log10(value));
  normalized = value / scale;
  if normalized < sqrt(2)
    nice = 1;
  elseif normalized < sqrt(10)
    nice = 2;
  elseif normalized <= 5.8
    nice = 5;
  else
    nice = 10;
  end
  step = nice * scale;
end
%=============================================================================
