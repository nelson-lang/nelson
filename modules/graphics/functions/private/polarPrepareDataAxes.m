%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ax = polarPrepareDataAxes(ax)
  if ~polarIsAxes(ax)
    ax = polarInitializeAxes(ax, false);
  end
  nextPlot = ax.NextPlot;
  state = polarGetState(ax, false);
  if strcmp(nextPlot, 'add')
    return
  end
  if isempty(state.DataHandles) && numel(allchild(ax)) == numel(state.GridHandles)
    return
  end
  newplot(ax);
  ax = polarInitializeAxes(ax, false);
  state = polarGetState(ax, false);
  state.GridHandles = gobjects(0, 1);
  state.DataHandles = gobjects(0, 1);
  polarSetState(ax, state);
end
%=============================================================================
