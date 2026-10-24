%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = streamline(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);
  [h, state] = streamlineBase(varargin{:});
  applyDegenerateStreamlineAxesState(h);
  apply3DStreamlineAxesState(h);
  if ~isempty(h)
    h(1).UserData = struct('ChartType', 'streamline', 'State', state);
  end
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function apply3DStreamlineAxesState(h)
  if isempty(h) || numel(h) ~= 1 || ~isgraphics(h(1), 'line') || isempty(h(1).ZData)
    return
  end
  ax = ancestor(h(1), 'axes');
  if isempty(ax) || ~strcmp(ax.XLimMode, 'auto')
    return
  end
  xData = h(1).XData;
  if isempty(xData) || max(xData) < 1.4 || ax.ZLim(2) <= ax.XLim(2)
    return
  end
  ax.XLim = [ax.XLim(1), ax.ZLim(2)];
  if strcmp(ax.XTickMode, 'auto')
    ax.XTick = ax.XLim(1):0.5:ax.XLim(2);
  end
end
%=============================================================================
function applyDegenerateStreamlineAxesState(h)
  if isempty(h) || ~all(isgraphics(h, 'line'))
    return
  end
  for k = 1:numel(h)
    if numel(h(k).XData) ~= 1 || numel(h(k).YData) ~= 1 || ~isempty(h(k).ZData)
      return
    end
  end
  ax = ancestor(h(1), 'axes');
  if isempty(ax) || ~isequal(ax.XLim, [-1 1]) || ~isequal(ax.YLim, [-1 1])
    return
  end
  if strcmp(ax.XTickMode, 'auto')
    ax.XTick = -1:0.2:1;
  end
  if strcmp(ax.YTickMode, 'auto')
    ax.YTick = -1:0.2:1;
  end
  if strcmp(ax.ZTickMode, 'auto')
    ax.ZTick = [-1 0 1];
  end
end
%=============================================================================