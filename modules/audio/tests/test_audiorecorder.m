%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
% <--AUDIO INPUT REQUIRED-->
% <--AUDIO OUTPUT REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
Fs = 44100;        % Sample rate (Hz)
nBits = 16;        % Bit depth
nChannels = 1;     % Mono recording
duration = 3;      % Recording duration (seconds)
recorder = audiorecorder(Fs, nBits, nChannels);
pause(2);
recordblocking(recorder, duration);
audioData = getaudiodata(recorder);
fprintf('Length: %d samples\n', length(audioData));
fprintf('Duration: %.2f seconds\n', length(audioData)/Fs);
fprintf('Max Amplitude: %.3f\n', max(abs(audioData)));
fprintf('Mean Amplitude: %.3f\n\n', mean(abs(audioData)));

play(recorder);

% Display recorder properties
fprintf('\nRecorder Properties:\n');
fprintf('Sample Rate: %d Hz\n', recorder.SampleRate);
fprintf('Bits Per Sample: %d\n', recorder.BitsPerSample);
fprintf('Number of Channels: %d\n', recorder.NumChannels);
