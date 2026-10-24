%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = surfc(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [ax, args] = parseSurfaceParent(args);
  saveHold = ishold(ax);
  s = surf(ax, args{:});
  hold(ax, 'on');
  [~, c] = contour(ax, s.XData, s.YData, s.ZData, 'ZLocation', 'zmin');
  applySurfaceContourZLimits(ax, s.ZData);
  if ~saveHold
    hold(ax, 'off');
  end
  view(ax, 3);
  grid(ax, 'on');
  if nargout == 1
    varargout{1} = [s; c];
  end
end
%=============================================================================
function [ax, args] = parseSurfaceParent(args)
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    ax = newplot(args{1});
    args = args(2:end);
  else
    [parent, args] = graphicsExtractParentProperty(args);
    ax = newplotTarget(parent);
  end
end
%=============================================================================
%=============================================================================
function applySurfaceContourZLimits(ax, zData)
  zValues = zData(isfinite(zData));
  if isempty(zValues)
    return
  end
  limits = [min(zValues(:)) max(zValues(:))];
  if limits(1) == limits(2)
    limits = limits + [-0.5 0.5];
  end
  zlim(ax, limits);
  ticks = surfaceContourZTicks(limits);
  if ~isempty(ticks)
    ax.ZTick = ticks;
  end
end
%=============================================================================
function ticks = surfaceContourZTicks(limits)
  span = limits(2) - limits(1);
  if ~isfinite(span) || span <= 0
    ticks = [];
    return
  end
  rawStep = span / 10;
  magnitude = 10 ^ floor(log10(rawStep));
  scaled = rawStep / magnitude;
  if scaled <= 1.2
    multiplier = 1;
  elseif scaled <= 2.5
    multiplier = 2;
  elseif scaled <= 5
    multiplier = 5;
  else
    multiplier = 10;
  end
  step = multiplier * magnitude;
  first = ceil(limits(1) / step) * step;
  last = floor(limits(2) / step) * step;
  if first > last
    ticks = [];
  else
    ticks = first:step:last;
  end
end