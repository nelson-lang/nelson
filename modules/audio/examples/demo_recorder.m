%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================

% Audio Recording Demo in Nelson
% This script demonstrates how to record, play, and analyze audio

clear; clc;

%% 1. Setup Audio Recorder
% Parameters
Fs = 44100;        % Sample rate (Hz)
nBits = 16;        % Bit depth
nChannels = 1;     % Mono recording
duration = 3;      % Recording duration (seconds)

% Create audiorecorder object
recorder = audiorecorder(Fs, nBits, nChannels);

fprintf('Audio Recorder Configuration:\n');
fprintf('Sample Rate: %d Hz\n', Fs);
fprintf('Bit Depth: %d bits\n', nBits);
fprintf('Channels: %d (Mono)\n', nChannels);
fprintf('Duration: %d seconds\n\n', duration);

%% 2. Record Audio
fprintf('Recording will start in 2 seconds...\n');
pause(2);

fprintf('Recording NOW! Speak into your microphone...\n');
recordblocking(recorder, duration);
fprintf('Recording complete!\n\n');

%% 3. Get Audio Data
audioData = getaudiodata(recorder);

fprintf('Audio Data Statistics:\n');
fprintf('Length: %d samples\n', length(audioData));
fprintf('Duration: %.2f seconds\n', length(audioData)/Fs);
fprintf('Max Amplitude: %.3f\n', max(abs(audioData)));
fprintf('Mean Amplitude: %.3f\n\n', mean(abs(audioData)));

%% 4. Play Back the Recording
fprintf('Playing back your recording...\n');
play(recorder);
pause(duration + 0.5);
fprintf('Playback complete!\n\n');

%% 5. Visualize the Audio
figure('Name', 'Audio Recording Analysis', 'Position', [100 100 1000 400]);

% Time domain plot
subplot(1,2,1);
t = (0:length(audioData)-1) / Fs;
plot(t, audioData, 'b', 'LineWidth', 1);
xlabel('Time (seconds)');
ylabel('Amplitude');
title('Time Domain Signal');
grid on;

% Frequency spectrum
subplot(1,2,2);
N = length(audioData);
f = (0:N-1) * (Fs/N);
Y = fft(audioData);
magnitude = abs(Y/N);
plot(f(1:N/2), magnitude(1:N/2), 'r', 'LineWidth', 1);
xlabel('Frequency (Hz)');
ylabel('Magnitude');
title('Frequency Spectrum');
grid on;
xlim([0 5000]); % Show up to 5kHz

% Display recorder properties
fprintf('\nRecorder Properties:\n');
fprintf('Sample Rate: %d Hz\n', recorder.SampleRate);
fprintf('Bits Per Sample: %d\n', recorder.BitsPerSample);
fprintf('Number of Channels: %d\n', recorder.NumChannels);

fprintf('\n--- Demo Complete! ---\n');
% End of demo_recorder.m