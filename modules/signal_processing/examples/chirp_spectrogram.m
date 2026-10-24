%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Reveal the changing frequency of a chirp with a short-time spectrum.
sampleRate = 1000;
duration = 2;
sampleTimes = (0:1 / sampleRate:duration - 1 / sampleRate).';
chirpSignal = chirp(sampleTimes, 40, duration, 320, 'linear');
analysisWindow = hann(128);
[spectrogramValues, frequencyValues, segmentTimes, powerValues] = spectrogram(...
  chirpSignal, analysisWindow, 96, 256, sampleRate);
powerDecibels = 10 * log10(powerValues + eps);
ridgeIndices = zeros(1, size(powerValues, 2));
for segmentIndex = 1:size(powerValues, 2)
  segmentPower = powerValues(:, segmentIndex);
  peakPower = max(segmentPower);
  ridgeIndices(segmentIndex) = find(segmentPower == peakPower, 1, 'first');
end
ridgeFrequencies = frequencyValues(ridgeIndices);

if ~exist('chirpSpectrogramFigureVisible', 'var')
  chirpSpectrogramFigureVisible = 'on';
end
figureHandle = figure(...
  'Name', 'Chirp spectrogram', ...
  'NumberTitle', 'off', ...
  'Visible', chirpSpectrogramFigureVisible);

signalAxes = subplot(2, 1, 1, 'Parent', figureHandle);
plot(signalAxes, sampleTimes, chirpSignal, ...
  'Color', [0.20 0.45 0.75], 'LineWidth', 1);
xlabel(signalAxes, 'Time (s)');
ylabel(signalAxes, 'Amplitude');
title(signalAxes, 'Linear chirp');
grid(signalAxes, 'on');

spectrogramAxes = subplot(2, 1, 2, 'Parent', figureHandle);
image('Parent', spectrogramAxes, ...
  'XData', [segmentTimes(1) segmentTimes(end)], ...
  'YData', [frequencyValues(1) frequencyValues(end)], ...
  'CData', powerDecibels, ...
  'CDataMapping', 'scaled');
hold(spectrogramAxes, 'on');
plot(spectrogramAxes, segmentTimes, ridgeFrequencies, 'w-', 'LineWidth', 1.2);
hold(spectrogramAxes, 'off');
set(spectrogramAxes, 'YDir', 'normal');
xlabel(spectrogramAxes, 'Time (s)');
ylabel(spectrogramAxes, 'Frequency (Hz)');
title(spectrogramAxes, 'Short-time power spectrum');
colormap(figureHandle, turbo(128));
%=============================================================================
