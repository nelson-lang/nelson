%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ax = polarInitializeAxes(ax, refreshNow)
  if nargin < 2
    refreshNow = true;
  end
  state = polarGetState(ax, false);
  maxRadius = max(abs(state.RLim));
  [xLimits, yLimits] = polarVisibleLimits(state.ThetaLim, maxRadius, state.ThetaAxisUnits);
  set(ax, 'PolarAxes', 'on', 'Visible', 'off', 'Box', 'off', ...
    'Color', [1 1 1], 'XTick', [], 'YTick', [], ...
    'DataAspectRatio', [1 1 1], 'PlotBoxAspectRatio', [1 1 1], ...
    'XLim', xLimits, 'YLim', yLimits);
  polarSetState(ax, state);
  if refreshNow
    polarRefresh(ax, state, false);
  end
end
%=============================================================================
