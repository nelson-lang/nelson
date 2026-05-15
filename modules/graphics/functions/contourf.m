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
    ax = newplot();
  end
  axes(ax);

  h = __contour__(contourProperties{:});
  axes(ax);
  if (strcmp(ax.XLimMode, 'auto') && strcmp(ax.YLimMode, 'auto'))
    limits = [min(h.XData(:)), max(h.XData(:)), min(h.YData(:)), max(h.YData(:))];
    if ~isempty(limits)
      axis(limits);
    end
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
