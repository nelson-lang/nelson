%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function completeUiControlReset(source, event)
  figureHandle = ancestor(source, 'figure');
  if isempty(figureHandle) || ~isgraphics(figureHandle)
    return
  end
  state = figureHandle.UserData;
  state.amplitudeEdit.String = '1.0';
  state.frequencySlider.Value = 1;
  state.phaseMenu.Value = 1;
  state.gridCheckbox.Value = 1;
  state.markerCheckbox.Value = 0;
  state.sineRadio.Value = 1;
  state.cosineRadio.Value = 0;
  state.colorList.Value = 1;
  state.thickToggle.Value = 0;
  completeUiControlUpdate(state.amplitudeEdit, []);
end
%=============================================================================
