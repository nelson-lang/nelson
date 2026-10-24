%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = contourf(varargin)
  % contourf(ax, ...)
  % contourf(Z)
  % contourf(X, Y, Z)
  % contourf(..., levels)
  % contourf(..., LineSpec)
  % contourf(..., Name, Value)
  % M = contourf(...)
  % [M, c] = contourf(...)
  [ax, contourProperties] = parseContourArguments(varargin, true);
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
