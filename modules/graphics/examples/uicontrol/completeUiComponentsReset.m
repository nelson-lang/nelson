%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function completeUiComponentsReset(source, event)
  figureHandle = ancestor(source, 'figure');
  if isempty(figureHandle) || ~isgraphics(figureHandle)
    return
  end
  state = figureHandle.UserData;
  state.amplitudeField.Value = 1;
  state.frequencySlider.Value = 1;
  state.waveformDropdown.Value = 'Sine';
  state.phaseSpinner.Value = 0;
  state.gridCheckbox.Value = true;
  state.solidRadio.Value = true;
  state.visibilitySwitch.Value = 'Visible';
  completeUiComponentsUpdate(state.amplitudeField, []);
end
%=============================================================================
