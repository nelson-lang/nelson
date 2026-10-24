%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function state = polarDefaultState()
  state = struct();
  state.PolarAxes = true;
  state.RLim = [0 1];
  state.RLimMode = 'auto';
  state.ThetaAxisUnits = 'degrees';
  state.ThetaLim = [0 360];
  state.ThetaLimMode = 'auto';
  state.RTick = 0:0.2:1;
  state.RTickMode = 'auto';
  state.ThetaTick = 0:30:360;
  state.ThetaTickMode = 'auto';
  state.RTickLabel = polarFormatTickLabels(state.RTick, false);
  state.RTickLabelMode = 'auto';
  state.ThetaTickLabel = polarThetaTickLabels(state.ThetaTick, state.ThetaLim, state.ThetaAxisUnits);
  state.ThetaTickLabelMode = 'auto';
  state.GridVisible = 'on';
  state.GridHandles = gobjects(0, 1);
  state.DataHandles = gobjects(0, 1);
end
%=============================================================================
