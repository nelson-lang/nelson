%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Identify the dominant frequencies in a synthetic sampled signal.
sampleRate = 200;
t = (0:399) / sampleRate;
signal = sin(2 * pi * 12 * t) + 0.45 * sin(2 * pi * 35 * t);
spectrum = abs(fft(signal));
frequency = (0:length(signal) - 1) * sampleRate / length(signal);
figure();
plot(frequency(1:200), spectrum(1:200));
xlabel('Frequency (Hz)');
ylabel('Magnitude');
