%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function completeUiControlUpdate(source, event)
  figureHandle = ancestor(source, 'figure');
  if isempty(figureHandle) || ~isgraphics(figureHandle)
    return
  end
  state = figureHandle.UserData;
  selectWaveform(source, state);
  amplitude = validatedAmplitude(state.amplitudeEdit);
  frequency = state.frequencySlider.Value;
  phaseValues = [0 pi / 4 pi / 2 pi];
  phase = phaseValues(state.phaseMenu.Value);
  [y, waveformLabel] = completeUiControlWaveform(...
    state, amplitude, frequency, phase);
  state.plot.YData = y;
  state.plot.Color = state.colors(state.colorList.Value, :);
  applyMarker(state);
  applyLineWidth(state);
  applyGrid(state);
  state.frequencyText.String = sprintf('%.2f', frequency);
  state.statusText.String = sprintf(...
    '%s: amplitude %.2f, frequency %.2f, phase %.2f rad', ...
    waveformLabel, amplitude, frequency, phase);
  title(state.axes, 'Classic UI controls working together');
  drawnow();
end

function selectWaveform(source, state)
  if isequal(source, state.sineRadio)
    state.sineRadio.Value = 1;
    state.cosineRadio.Value = 0;
  elseif isequal(source, state.cosineRadio)
    state.sineRadio.Value = 0;
    state.cosineRadio.Value = 1;
  end
end

function amplitude = validatedAmplitude(amplitudeEdit)
  amplitude = str2double(amplitudeEdit.String);
  if ~isfinite(amplitude) || amplitude <= 0
    amplitude = 1;
    amplitudeEdit.String = '1.0';
  end
end

function applyMarker(state)
  if state.markerCheckbox.Value
    state.plot.Marker = 'o';
    state.plot.MarkerIndices = 1:12:numel(state.x);
  else
    state.plot.Marker = 'none';
  end
end

function applyLineWidth(state)
  if state.thickToggle.Value
    state.plot.LineWidth = 4;
  else
    state.plot.LineWidth = 2;
  end
end

function applyGrid(state)
  if state.gridCheckbox.Value
    grid(state.axes, 'on');
  else
    grid(state.axes, 'off');
  end
end
%=============================================================================
