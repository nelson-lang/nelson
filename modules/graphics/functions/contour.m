%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = contour(varargin)
  % contour(ax, ...)
  % contour(Z)
  % contour(X, Y, Z)
  % contour(..., levels)
  % contour(..., LineSpec)
  % M = contour(...)
  % [M, c] = contour(...)
  [ax, contourProperties] = parseContourArguments(varargin, false);
  if isempty(ax)
    ax = graphicsParentProperty(contourProperties);
  end
  ax = newplotTarget(ax);
  nextPlot = ax.NextPlot;

  h = __contour__('Parent', ax, contourProperties{:});
  if (strcmp(ax.XLimMode, 'auto') && strcmp(ax.YLimMode, 'auto'))
    applyContourAxesState(ax, h);
  end
  if any(strcmp(nextPlot, {'replace', 'replaceall'}))
    ax.Box = 'on';
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
