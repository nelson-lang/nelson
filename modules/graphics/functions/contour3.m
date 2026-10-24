%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = contour3(varargin)
  [M, h] = contour(varargin{:});
  h.Floating = 'on';

  if ~ishold
    ax = ancestor(h, 'axes');
    view(ax, 3);
    applyContour3Ticks(ax);
    grid(ax, 'on');
  end
  if nargout > 0
    varargout{1} = h.ContourMatrix;
  else
    varargout = {};
  end
  if nargout > 1
    varargout{2} = h;
  end
end
%=============================================================================
function applyContour3Ticks(ax)
  xLimits = ax.XLim;
  step = contour3TickStep(diff(xLimits));
  ax.XTick = contour3Ticks(xLimits, step);
  yLimits = ax.YLim;
  step = contour3TickStep(diff(yLimits));
  ax.YTick = contour3Ticks(yLimits, step);
end
%=============================================================================
function ticks = contour3Ticks(limits, step)
  first = ceil(limits(1) / step) * step;
  last = floor(limits(2) / step) * step;
  ticks = first:step:last;
  ticks(abs(ticks) < step * 1e-10) = 0;
end
%=============================================================================
function step = contour3TickStep(span)
  if span <= 2.5
    step = 0.5;
  else
    step = 2;
  end
end
%=============================================================================