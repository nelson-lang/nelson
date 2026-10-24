%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function polarSetState(ax, state)
  if isprop(ax, 'RLim')
    args = {'UserData', state};
    args = addModeValue(args, 'RLim', state.RLim, 'RLimMode', state.RLimMode);
    args = addModeValue(args, 'RTick', state.RTick, 'RTickMode', state.RTickMode);
    args = addModeValue(args, 'RTickLabel', state.RTickLabel, ...
      'RTickLabelMode', state.RTickLabelMode);
    args = [args, {'ThetaAxisUnits', state.ThetaAxisUnits}];
    args = addModeValue(args, 'ThetaLim', state.ThetaLim, ...
      'ThetaLimMode', state.ThetaLimMode);
    args = addModeValue(args, 'ThetaTick', state.ThetaTick, ...
      'ThetaTickMode', state.ThetaTickMode);
    args = addModeValue(args, 'ThetaTickLabel', state.ThetaTickLabel, ...
      'ThetaTickLabelMode', state.ThetaTickLabelMode);
    set(ax, args{:});
  else
    ax.UserData = state;
  end
end
%=============================================================================
function args = addModeValue(args, valueName, value, modeName, modeValue)
  if strcmp(modeValue, 'auto')
    args = [args, {modeName, 'manual'}];
  end
  args = [args, {valueName, value, modeName, modeValue}];
end
%=============================================================================
