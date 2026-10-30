%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Stream a noisy ECG, smooth it with movmean and detect beats with islocalmax.
if ~exist('scopeFigureVisible', 'var')
  scopeFigureVisible = 'on';
end
if ~exist('scopeAnimationEnabled', 'var')
  scopeAnimationEnabled = true;
end
if ~exist('scopeAnimationFrames', 'var')
  scopeAnimationFrames = 900;
end
if ~exist('scopeFramePause', 'var')
  scopeFramePause = 0;
end
if ~exist('scopeAnimationContinuous', 'var')
  scopeAnimationContinuous = strcmp(scopeFigureVisible, 'on');
end
if ~exist('scopeSampleRate', 'var')
  scopeSampleRate = 250;
end
if ~exist('scopeBpm', 'var')
  scopeBpm = 72;
end
if ~exist('scopeSeed', 'var')
  scopeSeed = 7;
end
rng(scopeSeed);

scopeWindowSeconds = 5;
scopeWindowLength = scopeWindowSeconds * scopeSampleRate;
scopeAdvance = 6;
% Precompute a long ECG so the window can scroll smoothly across it.
scopeTotalSamples = scopeWindowLength + scopeAdvance * ...
  (scopeAnimationFrames + 2);
scopeSignal = scopeSynthesizeEcg(scopeTotalSamples, scopeSampleRate, ...
  scopeBpm);
scopeTimeAxis = (0:scopeWindowLength - 1) / scopeSampleRate;

scopeFigure = figure('Name', 'Live oscilloscope', 'NumberTitle', 'off', ...
  'Color', [0.04 0.06 0.05], 'Visible', scopeFigureVisible, ...
  'Position', [90 120 1080 560]);
scopeAxes = axes('Parent', scopeFigure, 'Position', [0.07 0.14 0.88 0.78], ...
  'Color', [0.03 0.05 0.04]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(scopeAxes);
hold(scopeAxes, 'on');
scopeRawLine = plot(scopeAxes, scopeTimeAxis, zeros(1, scopeWindowLength), ...
  'Color', [0.25 0.45 0.35], 'LineWidth', 0.8);
scopeSmoothLine = plot(scopeAxes, scopeTimeAxis, ...
  zeros(1, scopeWindowLength), 'Color', [0.30 0.95 0.55], 'LineWidth', 1.6);
scopePeakMarks = plot(scopeAxes, nan, nan, 'o', 'MarkerSize', 9, ...
  'MarkerEdgeColor', [1.0 0.85 0.3], 'MarkerFaceColor', 'none', ...
  'LineWidth', 1.5, 'LineStyle', 'none');
hold(scopeAxes, 'off');
set(scopeAxes, 'XColor', [0.6 0.7 0.64], 'YColor', [0.6 0.7 0.64]);
set(scopeAxes, 'XLim', [0 scopeWindowSeconds], 'YLim', [-0.6 1.3]);
grid(scopeAxes, 'on');
xlabel(scopeAxes, 'Time (s)');
ylabel(scopeAxes, 'Amplitude (mV)');
scopeTitle = title(scopeAxes, 'Live ECG: raw, smoothed and detected beats', ...
  'Color', [0.90 0.96 0.92], 'FontSize', 13);
scopeInfoText = uicontrol('Parent', scopeFigure, 'Style', 'text', ...
  'String', '', 'Position', [16 8 900 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.04 0.06 0.05], 'ForegroundColor', [0.7 0.9 0.78], ...
  'FontSize', 10);

if scopeAnimationEnabled
  scopeStart = 1;
  scopeFrame = 0;
  while isgraphics(scopeFigure)
    scopeFrame = scopeFrame + 1;
    if scopeFrame > scopeAnimationFrames && ~scopeAnimationContinuous
      break
    end
    if ~isgraphics(scopeFigure)
      break
    end
    try
      scopeStart = scopeStart + scopeAdvance;
      if scopeStart + scopeWindowLength - 1 > numel(scopeSignal)
        scopeStart = 1;
      end
      window = scopeSignal(scopeStart:scopeStart + scopeWindowLength - 1);
      smoothed = movmean(window, 5);
      % islocalmax finds the R peaks; a height gate rejects small ripples.
      peaks = islocalmax(smoothed) & (smoothed > 0.55);
      peakTimes = scopeTimeAxis(peaks);
      set(scopeRawLine, 'YData', window);
      set(scopeSmoothLine, 'YData', smoothed);
      set(scopePeakMarks, 'XData', peakTimes, 'YData', smoothed(peaks));
      bpm = 0;
      if numel(peakTimes) >= 2
        bpm = 60 / mean(diff(peakTimes));
      end
      if isgraphics(scopeInfoText)
        set(scopeInfoText, 'String', sprintf(...
          'window %d samples | %d beats detected | heart rate %.0f bpm', ...
          scopeWindowLength, numel(peakTimes), bpm));
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if scopeFramePause > 0
      pause(scopeFramePause);
    end
  end
end
%=============================================================================
function signal = scopeSynthesizeEcg(sampleCount, sampleRate, bpm)
  time = (0:sampleCount - 1) / sampleRate;
  beatPeriod = 60 / bpm;
  signal = zeros(1, sampleCount);
  % Place one ECG complex per beat, with a little natural interval jitter.
  beatTime = 0.4;
  while beatTime < time(end)
    signal = signal + scopeEcgComplex(time - beatTime);
    beatTime = beatTime + beatPeriod * (1 + 0.04 * randn());
  end
  % Add baseline wander and measurement noise.
  signal = signal + 0.04 * sin(2 * pi * 0.25 * time) + 0.02 * randn(1, ...
    sampleCount);
end
%=============================================================================
function wave = scopeEcgComplex(tau)
  % One P-QRS-T complex built from Gaussian bumps centred on the R peak.
  wave = 0.10 * exp(-((tau + 0.20) / 0.025) .^ 2) + ...
    -0.12 * exp(-((tau + 0.03) / 0.008) .^ 2) + ...
    1.00 * exp(-(tau / 0.009) .^ 2) + ...
    -0.22 * exp(-((tau - 0.03) / 0.010) .^ 2) + ...
    0.30 * exp(-((tau - 0.22) / 0.040) .^ 2);
end
%=============================================================================
