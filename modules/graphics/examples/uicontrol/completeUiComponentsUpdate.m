%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function completeUiComponentsUpdate(source, event)
  figureHandle = ancestor(source, 'figure');
  if isempty(figureHandle) || ~isgraphics(figureHandle)
    return
  end
  state = figureHandle.UserData;
  synchronizeFrequencyControls(source, state);
  amplitude = state.amplitudeField.Value;
  frequency = state.frequencySlider.Value;
  phase = state.phaseSpinner.Value * pi / 180;
  argument = frequency * state.x + phase;
  waveform = state.waveformDropdown.Value;
  state.plot.YData = waveformValues(waveform, amplitude, argument);
  applyLineStyle(state);
  applyGrid(state);
  applyVisibility(state);
  state.frequencyGauge.Value = frequency;
  state.statusLabel.Text = sprintf(...
    '%s - amplitude %.2f, frequency %.2f, phase %.0f degrees', ...
    waveform, amplitude, frequency, state.phaseSpinner.Value);
  title(state.axes, 'Modern UI components working together');
  drawnow();
end

function synchronizeFrequencyControls(source, state)
  if isequal(source, state.frequencyKnob)
    state.frequencySlider.Value = state.frequencyKnob.Value;
  else
    state.frequencyKnob.Value = state.frequencySlider.Value;
  end
end

function y = waveformValues(waveform, amplitude, argument)
  if strcmp(waveform, 'Sine')
    y = amplitude * sin(argument);
  elseif strcmp(waveform, 'Cosine')
    y = amplitude * cos(argument);
  elseif strcmp(waveform, 'Square')
    y = amplitude * sign(sin(argument));
  else
    y = amplitude * (2 / pi) * asin(sin(argument));
  end
end

function applyLineStyle(state)
  if state.solidRadio.Value
    state.plot.LineStyle = '-';
  else
    state.plot.LineStyle = '--';
  end
end

function applyGrid(state)
  if state.gridCheckbox.Value
    grid(state.axes, 'on');
  else
    grid(state.axes, 'off');
  end
end

function applyVisibility(state)
  if strcmp(state.visibilitySwitch.Value, 'Visible')
    state.plot.Visible = 'on';
    state.statusLamp.Color = [0.20 0.75 0.30];
  else
    state.plot.Visible = 'off';
    state.statusLamp.Color = [0.85 0.20 0.20];
  end
end
%=============================================================================
