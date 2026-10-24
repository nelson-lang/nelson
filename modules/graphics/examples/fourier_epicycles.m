%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Draw a closed shape with a chain of rotating Fourier epicycles.
if ~exist('epicyclesFigureVisible', 'var')
  epicyclesFigureVisible = 'on';
end
if ~exist('epicyclesBatch', 'var')
  epicyclesBatch = false;
end
if ~exist('epicyclesMaxFrames', 'var')
  epicyclesMaxFrames = 360;
end
if ~exist('epicyclesFramePause', 'var')
  epicyclesFramePause = 0;
end
if ~exist('epicyclesOutputImage', 'var')
  epicyclesOutputImage = '';
end

epicyclesSampleCount = 256;
epicyclesFramesPerPeriod = 360;
epicyclesContour = epicyclesHeartContour(epicyclesSampleCount);
[epicyclesCoeff, epicyclesFreq] = epicyclesCoefficients(epicyclesContour);
epicyclesMaxHarmonics = min(numel(epicyclesCoeff), 80);
if ~exist('epicyclesHarmonics', 'var')
  epicyclesHarmonics = min(40, epicyclesMaxHarmonics);
end
epicyclesHarmonics = max(1, min(epicyclesMaxHarmonics, round(epicyclesHarmonics)));

epicyclesRange = max(max(abs(real(epicyclesContour))), ...
  max(abs(imag(epicyclesContour))));
epicyclesLimit = 1.35 * epicyclesRange;
epicyclesCenterX = mean(real(epicyclesContour));
epicyclesCenterY = mean(imag(epicyclesContour));

epicyclesFigure = figure('Name', 'Fourier epicycles', ...
  'NumberTitle', 'off', 'Color', [0.06 0.07 0.11], ...
  'Visible', epicyclesFigureVisible, 'Position', [80 80 900 760]);
epicyclesAxes = axes('Parent', epicyclesFigure, ...
  'Position', [0.06 0.16 0.88 0.78], 'Color', [0.09 0.10 0.15]);
hold(epicyclesAxes, 'on');
plot(epicyclesAxes, real(epicyclesContour([1:end 1])), ...
  imag(epicyclesContour([1:end 1])), 'Color', [0.22 0.26 0.36], ...
  'LineWidth', 1.0);
epicyclesCircleLine = plot(epicyclesAxes, NaN, NaN, ...
  'Color', [0.45 0.52 0.70], 'LineWidth', 0.7);
epicyclesChainLine = plot(epicyclesAxes, NaN, NaN, ...
  'Color', [0.75 0.82 0.95], 'LineWidth', 1.4, ...
  'Marker', 'o', 'MarkerSize', 3, ...
  'MarkerFaceColor', [0.90 0.94 1.00], 'MarkerEdgeColor', 'none');
epicyclesPenTrail = animatedline(epicyclesAxes, ...
  'Color', [1.00 0.42 0.62], 'LineWidth', 2.0);
epicyclesPenMarker = plot(epicyclesAxes, NaN, NaN, 'o', ...
  'MarkerSize', 7, 'MarkerFaceColor', [1.00 0.42 0.62], ...
  'MarkerEdgeColor', [1.00 0.80 0.88]);
hold(epicyclesAxes, 'off');
axis(epicyclesAxes, 'equal');
axis(epicyclesAxes, [epicyclesCenterX - epicyclesLimit ...
  epicyclesCenterX + epicyclesLimit ...
  epicyclesCenterY - epicyclesLimit ...
  epicyclesCenterY + epicyclesLimit]);
set(epicyclesAxes, 'XLimMode', 'manual', 'YLimMode', 'manual', ...
  'XColor', [0.40 0.44 0.52], 'YColor', [0.40 0.44 0.52]);
axis(epicyclesAxes, 'off');
title(epicyclesAxes, 'Fourier epicycles', 'Color', [0.90 0.92 0.98]);

uicontrol('Parent', epicyclesFigure, 'Style', 'text', ...
  'String', 'Harmonics', 'Position', [20 66 90 22], ...
  'HorizontalAlignment', 'left', 'BackgroundColor', [0.06 0.07 0.11], ...
  'ForegroundColor', [0.85 0.88 0.95]);
epicyclesSlider = uicontrol('Parent', epicyclesFigure, 'Style', 'slider', ...
  'Min', 1, 'Max', epicyclesMaxHarmonics, 'Value', epicyclesHarmonics, ...
  'Position', [110 66 300 22]);
epicyclesValueText = uicontrol('Parent', epicyclesFigure, 'Style', 'text', ...
  'String', sprintf('%d / %d', epicyclesHarmonics, epicyclesMaxHarmonics), ...
  'Position', [420 66 90 22], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.06 0.07 0.11], 'ForegroundColor', [0.85 0.88 0.95]);
epicyclesStartButton = uicontrol('Parent', epicyclesFigure, ...
  'Style', 'pushbutton', 'String', 'Start', 'Position', [540 62 90 30], ...
  'UserData', false, ...
  'Callback', @(source, event) set(source, 'UserData', true));
epicyclesResetButton = uicontrol('Parent', epicyclesFigure, ...
  'Style', 'pushbutton', 'String', 'Reset', 'Position', [640 62 90 30], ...
  'UserData', false, ...
  'Callback', @(source, event) set(source, 'UserData', true));
epicyclesStopButton = uicontrol('Parent', epicyclesFigure, ...
  'Style', 'pushbutton', 'String', 'Stop', 'Position', [740 62 90 30], ...
  'UserData', false, ...
  'Callback', @(source, event) set(source, 'UserData', true));

epicyclesState = struct();
epicyclesState.axes = epicyclesAxes;
epicyclesState.circleLine = epicyclesCircleLine;
epicyclesState.chainLine = epicyclesChainLine;
epicyclesState.penTrail = epicyclesPenTrail;
epicyclesState.penMarker = epicyclesPenMarker;
epicyclesState.coeff = epicyclesCoeff;
epicyclesState.freq = epicyclesFreq;
epicyclesState.framesPerPeriod = epicyclesFramesPerPeriod;
epicyclesState.maxHarmonics = epicyclesMaxHarmonics;

if epicyclesBatch
  epicyclesFrameTotal = max(2, min(epicyclesMaxFrames, ...
    epicyclesFramesPerPeriod));
  for frameIndex = 1:epicyclesFrameTotal
    if ~isgraphics(epicyclesFigure)
      break
    end
    epicyclesRenderFrame(epicyclesState, frameIndex, epicyclesHarmonics);
    drawnow();
    if epicyclesFramePause > 0
      pause(epicyclesFramePause);
    end
  end
  if ~isempty(epicyclesOutputImage) && isgraphics(epicyclesFigure)
    drawnow();
    saveas(epicyclesFigure, epicyclesOutputImage);
  end
else
  epicyclesRunning = true;
  epicyclesFrameIndex = 1;
  while isgraphics(epicyclesFigure)
    if isgraphics(epicyclesSlider)
      epicyclesNewHarmonics = max(1, min(epicyclesMaxHarmonics, ...
        round(epicyclesSlider.Value)));
      if epicyclesNewHarmonics ~= epicyclesHarmonics
        epicyclesHarmonics = epicyclesNewHarmonics;
        if isgraphics(epicyclesValueText)
          set(epicyclesValueText, 'String', sprintf('%d / %d', ...
            epicyclesHarmonics, epicyclesMaxHarmonics));
        end
        clearpoints(epicyclesPenTrail);
        epicyclesFrameIndex = 1;
      end
    end
    if isgraphics(epicyclesStartButton) && epicyclesStartButton.UserData
      set(epicyclesStartButton, 'UserData', false);
      epicyclesRunning = true;
    end
    if isgraphics(epicyclesStopButton) && epicyclesStopButton.UserData
      set(epicyclesStopButton, 'UserData', false);
      epicyclesRunning = false;
    end
    if isgraphics(epicyclesResetButton) && epicyclesResetButton.UserData
      set(epicyclesResetButton, 'UserData', false);
      clearpoints(epicyclesPenTrail);
      epicyclesFrameIndex = 1;
    end
    if epicyclesRunning
      epicyclesRenderFrame(epicyclesState, epicyclesFrameIndex, ...
        epicyclesHarmonics);
      epicyclesFrameIndex = epicyclesFrameIndex + 1;
      if epicyclesFrameIndex > epicyclesFramesPerPeriod
        epicyclesFrameIndex = 1;
        clearpoints(epicyclesPenTrail);
      end
    end
    drawnow();
    if epicyclesFramePause > 0
      pause(epicyclesFramePause);
    else
      pause(0.001);
    end
  end
end
%=============================================================================
function contour = epicyclesHeartContour(sampleCount)
  angle = linspace(0, 2 * pi, sampleCount + 1);
  angle(end) = [];
  x = 16 * sin(angle) .^ 3;
  y = 13 * cos(angle) - 5 * cos(2 * angle) - ...
    2 * cos(3 * angle) - cos(4 * angle);
  contour = x + 1i * y;
end
%=============================================================================
function [coeff, freq] = epicyclesCoefficients(contour)
  sampleCount = numel(contour);
  spectrum = fft(contour) / sampleCount;
  wave = 0:sampleCount - 1;
  freq = wave;
  freq(wave > sampleCount / 2) = wave(wave > sampleCount / 2) - sampleCount;
  [~, order] = sort(abs(spectrum), 'descend');
  coeff = spectrum(order);
  freq = freq(order);
end
%=============================================================================
function [chainX, chainY] = epicyclesChain(coeff, freq, parameter, harmonics)
  vectors = coeff(1:harmonics) .* exp(1i * freq(1:harmonics) * parameter);
  positions = cumsum(vectors);
  chainX = [0 real(positions)];
  chainY = [0 imag(positions)];
end
%=============================================================================
function [circleX, circleY] = epicyclesCircles(centersX, centersY, radii)
  theta = linspace(0, 2 * pi, 24);
  harmonics = numel(radii);
  circleX = NaN(1, harmonics * (numel(theta) + 1));
  circleY = NaN(1, harmonics * (numel(theta) + 1));
  block = numel(theta) + 1;
  for index = 1:harmonics
    start = (index - 1) * block + 1;
    stop = start + numel(theta) - 1;
    circleX(start:stop) = centersX(index) + radii(index) * cos(theta);
    circleY(start:stop) = centersY(index) + radii(index) * sin(theta);
  end
end
%=============================================================================
function epicyclesRenderFrame(state, frameIndex, harmonics)
  if ~isgraphics(state.axes)
    return
  end
  parameter = 2 * pi * (frameIndex - 1) / state.framesPerPeriod;
  [chainX, chainY] = epicyclesChain(state.coeff, state.freq, ...
    parameter, harmonics);
  radii = abs(state.coeff(1:harmonics));
  [circleX, circleY] = epicyclesCircles(chainX(1:harmonics), ...
    chainY(1:harmonics), radii);
  if isgraphics(state.circleLine)
    set(state.circleLine, 'XData', circleX, 'YData', circleY);
  end
  if isgraphics(state.chainLine)
    set(state.chainLine, 'XData', chainX, 'YData', chainY);
  end
  if isgraphics(state.penMarker)
    set(state.penMarker, 'XData', chainX(end), 'YData', chainY(end));
  end
  if isgraphics(state.penTrail)
    addpoints(state.penTrail, chainX(end), chainY(end));
  end
end
%=============================================================================
