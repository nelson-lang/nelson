%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Animate a harmonograph tracing decaying Lissajous figures with a moving pen.
if ~exist('harmonographFigureVisible', 'var')
  harmonographFigureVisible = 'on';
end
if ~exist('harmonographBatch', 'var')
  harmonographBatch = false;
end
if ~exist('harmonographMaxFrames', 'var')
  harmonographMaxFrames = 400;
end
if ~exist('harmonographFramePause', 'var')
  harmonographFramePause = 0;
end
if ~exist('harmonographOutputImage', 'var')
  harmonographOutputImage = '';
end

harmonographDefaultFreqX = 2.00;
harmonographDefaultFreqY = 3.00;
harmonographDefaultDamping = 0.012;
harmonographBackground = [0.06 0.07 0.10];
harmonographPenColor = [0.16 0.66 0.94];
harmonographTipColor = [1.00 0.78 0.22];

harmonographFigure = figure('Name', 'Harmonograph', ...
  'NumberTitle', 'off', 'Color', harmonographBackground, ...
  'Visible', harmonographFigureVisible, 'Position', [80 80 1120 720]);

harmonographAxes = axes('Parent', harmonographFigure, ...
  'Units', 'normalized', 'Position', [0.05 0.16 0.90 0.78]);
hold(harmonographAxes, 'on');
set(harmonographAxes, 'Color', harmonographBackground, ...
  'DataAspectRatio', [1 1 1], 'XLim', [-2.2 2.2], 'YLim', [-2.2 2.2]);
axis(harmonographAxes, 'off');
title(harmonographAxes, 'Harmonograph', 'Color', [0.90 0.92 0.96]);

harmonographPenLine = animatedline(harmonographAxes, ...
  'Color', harmonographPenColor, 'LineWidth', 0.9);
harmonographPenTip = plot(harmonographAxes, 0, 0, 'o', ...
  'MarkerEdgeColor', [0.10 0.10 0.12], ...
  'MarkerFaceColor', harmonographTipColor, 'MarkerSize', 8, ...
  'LineStyle', 'none');
hold(harmonographAxes, 'off');

harmonographState = struct();
harmonographState.figure = harmonographFigure;
harmonographState.axes = harmonographAxes;
harmonographState.penLine = harmonographPenLine;
harmonographState.penTip = harmonographPenTip;

harmonographControlLabels = [0.90 0.92 0.96];
uicontrol('Parent', harmonographFigure, 'Style', 'text', ...
  'String', 'Freq X', 'HorizontalAlignment', 'left', ...
  'Position', [24 84 150 18], 'BackgroundColor', harmonographBackground, ...
  'ForegroundColor', harmonographControlLabels);
harmonographFreqXSlider = uicontrol('Parent', harmonographFigure, ...
  'Style', 'slider', 'Min', 1, 'Max', 6, ...
  'Value', harmonographDefaultFreqX, 'SliderStep', [0.01 0.10], ...
  'Position', [24 58 150 22], 'Tag', 'harmonographFreqX');
harmonographFreqXValue = uicontrol('Parent', harmonographFigure, ...
  'Style', 'text', 'String', sprintf('%.2f', harmonographDefaultFreqX), ...
  'Position', [178 58 46 22], 'BackgroundColor', harmonographBackground, ...
  'ForegroundColor', harmonographControlLabels);

uicontrol('Parent', harmonographFigure, 'Style', 'text', ...
  'String', 'Freq Y', 'HorizontalAlignment', 'left', ...
  'Position', [250 84 150 18], 'BackgroundColor', harmonographBackground, ...
  'ForegroundColor', harmonographControlLabels);
harmonographFreqYSlider = uicontrol('Parent', harmonographFigure, ...
  'Style', 'slider', 'Min', 1, 'Max', 6, ...
  'Value', harmonographDefaultFreqY, 'SliderStep', [0.01 0.10], ...
  'Position', [250 58 150 22], 'Tag', 'harmonographFreqY');
harmonographFreqYValue = uicontrol('Parent', harmonographFigure, ...
  'Style', 'text', 'String', sprintf('%.2f', harmonographDefaultFreqY), ...
  'Position', [404 58 46 22], 'BackgroundColor', harmonographBackground, ...
  'ForegroundColor', harmonographControlLabels);

uicontrol('Parent', harmonographFigure, 'Style', 'text', ...
  'String', 'Damping', 'HorizontalAlignment', 'left', ...
  'Position', [476 84 150 18], 'BackgroundColor', harmonographBackground, ...
  'ForegroundColor', harmonographControlLabels);
harmonographDampingSlider = uicontrol('Parent', harmonographFigure, ...
  'Style', 'slider', 'Min', 0.004, 'Max', 0.050, ...
  'Value', harmonographDefaultDamping, 'SliderStep', [0.01 0.10], ...
  'Position', [476 58 150 22], 'Tag', 'harmonographDamping');
harmonographDampingValue = uicontrol('Parent', harmonographFigure, ...
  'Style', 'text', 'String', sprintf('%.3f', harmonographDefaultDamping), ...
  'Position', [630 58 46 22], 'BackgroundColor', harmonographBackground, ...
  'ForegroundColor', harmonographControlLabels);

harmonographStartButton = uicontrol('Parent', harmonographFigure, ...
  'Style', 'pushbutton', 'String', 'Start', ...
  'Position', [724 52 110 34], 'UserData', false, ...
  'Callback', @(src, evt) set(src, 'UserData', true));
harmonographResetButton = uicontrol('Parent', harmonographFigure, ...
  'Style', 'pushbutton', 'String', 'Reset', ...
  'Position', [844 52 110 34], 'UserData', false, ...
  'Callback', @(src, evt) set(src, 'UserData', true));
harmonographStopButton = uicontrol('Parent', harmonographFigure, ...
  'Style', 'pushbutton', 'String', 'Stop', ...
  'Position', [964 52 110 34], 'UserData', false, ...
  'Callback', @(src, evt) set(src, 'UserData', true));

harmonographControls = struct();
harmonographControls.startButton = harmonographStartButton;
harmonographControls.resetButton = harmonographResetButton;
harmonographControls.stopButton = harmonographStopButton;
harmonographValueTexts = struct();
harmonographValueTexts.freqX = harmonographFreqXValue;
harmonographValueTexts.freqY = harmonographFreqYValue;
harmonographValueTexts.damping = harmonographDampingValue;

if harmonographBatch
  harmonographFrames = max(2, min(harmonographMaxFrames, 400));
  harmonographStatus = harmonographRunDraw(harmonographState, ...
    harmonographDefaultFreqX, harmonographDefaultFreqY, ...
    harmonographDefaultDamping, harmonographFrames, ...
    harmonographFramePause, [], harmonographValueTexts);
  if ~isempty(harmonographOutputImage) && isgraphics(harmonographFigure)
    drawnow();
    saveas(harmonographFigure, harmonographOutputImage);
  end
else
  harmonographFrames = max(2, min(harmonographMaxFrames, 600));
  harmonographPendingStart = true;
  while isgraphics(harmonographFigure)
    if ~harmonographPendingStart
      if ~harmonographWaitForStart(harmonographControls, ...
        harmonographFigure)
        break
      end
    end
    harmonographPendingStart = false;
    if ~isgraphics(harmonographFigure)
      break
    end
    freqX = harmonographFreqXSlider.Value;
    freqY = harmonographFreqYSlider.Value;
    damping = harmonographDampingSlider.Value;
    harmonographStatus = harmonographRunDraw(harmonographState, ...
      freqX, freqY, damping, harmonographFrames, ...
      harmonographFramePause, harmonographControls, ...
      harmonographValueTexts);
    if strcmp(harmonographStatus, 'closed')
      break
    end
  end
end
%=============================================================================
function status = harmonographRunDraw(state, freqX, freqY, damping, ...
  frames, framePause, controls, valueTexts)
  status = 'completed';
  hasControls = ~isempty(controls);
  if hasControls && ~isempty(valueTexts)
    if isgraphics(valueTexts.freqX)
      set(valueTexts.freqX, 'String', sprintf('%.2f', freqX));
    end
    if isgraphics(valueTexts.freqY)
      set(valueTexts.freqY, 'String', sprintf('%.2f', freqY));
    end
    if isgraphics(valueTexts.damping)
      set(valueTexts.damping, 'String', sprintf('%.3f', damping));
    end
  end
  [curveX, curveY] = harmonographCurve(freqX, freqY, damping);
  pointCount = numel(curveX);
  if ~harmonographHandlesValid(state)
    status = 'closed';
    return
  end
  clearpoints(state.penLine);
  set(state.penTip, 'XData', curveX(1), 'YData', curveY(1));
  extent = max(abs([curveX(:); curveY(:)]));
  if ~(extent > 0)
    extent = 1;
  end
  limit = 1.05 * extent;
  set(state.axes, 'DataAspectRatio', [1 1 1], ...
    'XLim', [-limit limit], 'YLim', [-limit limit]);
  axis(state.axes, 'off');
  title(state.axes, sprintf(...
    'Harmonograph   fX = %.2f   fY = %.2f   damping = %.3f', ...
    freqX, freqY, damping), 'Color', [0.90 0.92 0.96]);
  drawnow();
  stride = max(1, ceil(pointCount / frames));
  sampleIndices = unique([stride:stride:pointCount, pointCount]);
  previousIndex = 0;
  for stepIndex = 1:numel(sampleIndices)
    currentIndex = sampleIndices(stepIndex);
    if ~harmonographHandlesValid(state)
      status = 'closed';
      return
    end
    addpoints(state.penLine, ...
      curveX(previousIndex + 1:currentIndex), ...
      curveY(previousIndex + 1:currentIndex));
    set(state.penTip, 'XData', curveX(currentIndex), ...
      'YData', curveY(currentIndex));
    drawnow();
    if framePause > 0
      pause(framePause);
    end
    if hasControls
      if isgraphics(controls.stopButton) && controls.stopButton.UserData
        set(controls.stopButton, 'UserData', false);
        status = 'stopped';
        return
      end
      if isgraphics(controls.resetButton) && controls.resetButton.UserData
        set(controls.resetButton, 'UserData', false);
        if harmonographHandlesValid(state)
          clearpoints(state.penLine);
          drawnow();
        end
        status = 'reset';
        return
      end
    end
    previousIndex = currentIndex;
  end
end
%=============================================================================
function [curveX, curveY] = harmonographCurve(freqX, freqY, damping)
  % Keep the sampled pen path modest: the whole figure stays on screen, so the
  % per-frame redraw cost grows with the point count. 2600 samples keep the
  % decaying loops smooth while staying responsive.
  pointCount = 2600;
  timeSpan = 150;
  timeVector = linspace(0, timeSpan, pointCount);
  detune = 0.02;
  phase1 = 0.0;
  phase2 = pi / 2;
  phase3 = pi / 4;
  phase4 = 3 * pi / 4;
  envelope = exp(-damping * timeVector);
  curveX = (sin((freqX) * timeVector + phase1) + ...
    sin((freqX + detune) * timeVector + phase2)) .* envelope;
  curveY = (sin((freqY) * timeVector + phase3) + ...
    sin((freqY + detune) * timeVector + phase4)) .* envelope;
end
%=============================================================================
function keepWaiting = harmonographWaitForStart(controls, figureHandle)
  keepWaiting = true;
  if isgraphics(controls.startButton)
    set(controls.startButton, 'UserData', false);
  end
  while isgraphics(figureHandle)
    drawnow();
    pause(0.02);
    if isgraphics(controls.startButton) && controls.startButton.UserData
      set(controls.startButton, 'UserData', false);
      return
    end
  end
  keepWaiting = false;
end
%=============================================================================
function valid = harmonographHandlesValid(state)
  valid = isgraphics(state.figure) && isgraphics(state.axes) && ...
    isgraphics(state.penLine) && isgraphics(state.penTip);
end
%=============================================================================
