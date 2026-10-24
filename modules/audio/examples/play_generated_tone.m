%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Generate a short stereo tone, display its waveform and play it.
%=============================================================================
if ~exist('audioToneDuration', 'var')
  audioToneDuration = 0.6;
end
if ~exist('audioToneSampleRate', 'var')
  audioToneSampleRate = 22050;
end
if ~exist('audioToneFigureVisible', 'var')
  audioToneFigureVisible = 'on';
end
%=============================================================================
devices = audiodevinfo();
if isempty(devices.output)
  error('This example requires an audio output device.');
end
%=============================================================================
fs = audioToneSampleRate;
t = (0:round(audioToneDuration * fs) - 1)' / fs;
envelope = min(1, t / 0.04) .* exp(-3.5 * t / max(audioToneDuration, eps));
left = envelope .* (0.55 * sin(2 * pi * 440 * t) + 0.25 * sin(2 * pi * 660 * t));
right = envelope .* (0.55 * sin(2 * pi * 554.37 * t) + 0.25 * sin(2 * pi * 880 * t));
y = 0.8 * [left, right];
%=============================================================================
figure('Name', 'Generated stereo tone', 'NumberTitle', 'off', ...
  'Visible', audioToneFigureVisible);
plot(t, y);
grid on;
xlabel('Time (s)');
ylabel('Amplitude');
title('Generated stereo tone');
legend({'Left', 'Right'});
%=============================================================================
sound(y, fs);
%=============================================================================
