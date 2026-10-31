%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function state = polarGetState(ax, syncProperties)
  if nargin < 2
    syncProperties = true;
  end
  if ~(isgraphics(ax, 'axes') || isgraphics(ax, 'polaraxes'))
    error('Nelson:polarGetState:axesExpected', 'Axes expected.');
  end
  state = ax.UserData;
  if ~(isstruct(state) && isfield(state, 'PolarAxes') && state.PolarAxes)
    state = polarDefaultState();
    polarSetState(ax, state);
  end
  if syncProperties
    state = polarStateFromProperties(ax, state);
  end
end
%=============================================================================
function state = polarStateFromProperties(ax, state)
  defaults = polarDefaultState();
  defaultFields = fieldnames(defaults);
  for k = 1:numel(defaultFields)
    name = defaultFields{k};
    if ~isfield(state, name)
      state.(name) = defaults.(name);
    end
  end
  if isprop(ax, 'RLim')
    state.RLim = ax.RLim;
    state.RLimMode = ax.RLimMode;
    state.RTick = ax.RTick;
    state.RTickMode = ax.RTickMode;
    state.RTickLabel = ax.RTickLabel;
    state.RTickLabelMode = ax.RTickLabelMode;
    state.ThetaAxisUnits = ax.ThetaAxisUnits;
    state.ThetaLim = ax.ThetaLim;
    state.ThetaLimMode = ax.ThetaLimMode;
    state.ThetaTick = ax.ThetaTick;
    state.ThetaTickMode = ax.ThetaTickMode;
    state.ThetaTickLabel = ax.ThetaTickLabel;
    state.ThetaTickLabelMode = ax.ThetaTickLabelMode;
  end
  % Grid orientation and styling live on a polaraxes only; a polar-flagged plain
  % axes does not carry them, so each read is guarded.
  state.ThetaZeroLocation = guardedProperty(ax, 'ThetaZeroLocation', state.ThetaZeroLocation);
  state.ThetaDir = guardedProperty(ax, 'ThetaDir', state.ThetaDir);
  state.RColor = guardedProperty(ax, 'RColor', state.RColor);
  state.ThetaColor = guardedProperty(ax, 'ThetaColor', state.ThetaColor);
  state.RGrid = guardedProperty(ax, 'RGrid', state.RGrid);
  state.ThetaGrid = guardedProperty(ax, 'ThetaGrid', state.ThetaGrid);
  state.RDir = guardedProperty(ax, 'RDir', state.RDir);
  state.RAxisLocation = guardedProperty(ax, 'RAxisLocation', state.RAxisLocation);
  state.RMinorGrid = guardedProperty(ax, 'RMinorGrid', state.RMinorGrid);
  state.ThetaMinorGrid = guardedProperty(ax, 'ThetaMinorGrid', state.ThetaMinorGrid);
  state.RMinorTick = guardedProperty(ax, 'RMinorTick', state.RMinorTick);
  state.ThetaMinorTick = guardedProperty(ax, 'ThetaMinorTick', state.ThetaMinorTick);
end
%=============================================================================
function value = guardedProperty(ax, name, fallback)
  value = fallback;
  if isprop(ax, name)
    value = ax.(name);
  end
end
%=============================================================================
