%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ticks = polarNiceTicks(limits)
  rmin = limits(1);
  rmax = limits(2);
  if ~(isfinite(rmin) && isfinite(rmax)) || rmax <= rmin
    ticks = 0:0.2:1;
    return
  end
  span = rmax - rmin;
  rawStep = span / 5;
  magnitude = 10 ^ floor(log10(rawStep));
  normalized = rawStep / magnitude;
  if normalized <= 1
    step = magnitude;
  elseif normalized <= 2
    step = 2 * magnitude;
  elseif normalized <= 5
    step = 5 * magnitude;
  else
    step = 10 * magnitude;
  end
  firstTick = ceil(rmin / step) * step;
  lastTick = floor(rmax / step) * step;
  ticks = firstTick:step:lastTick;
  if isempty(ticks) || abs(ticks(1) - rmin) > 1e-12
    ticks = [rmin ticks];
  end
  if abs(ticks(end) - rmax) > 1e-12
    ticks = [ticks rmax];
  end
end
%=============================================================================
