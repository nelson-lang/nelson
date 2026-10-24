%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [s, f, t] = signalStftCore(x, window, overlapLength, fftLength, fs, frequencyRange)
  if nargin < 6
    frequencyRange = 'centered';
  end
  window = window(:);
  step = length(window) - overlapLength;
  starts = 1:step:(length(x) - length(window) + 1);
  s = zeros(fftLength, length(starts));
  for idx = 1:length(starts)
    frame = x(starts(idx):starts(idx) + length(window) - 1) .* window;
    padded = zeros(fftLength, 1);
    padded(1:length(frame)) = frame;
    s(:, idx) = fft(padded);
  end
  if strcmpi(frequencyRange, 'onesided')
    keep = 1:floor(fftLength / 2) + 1;
    s = s(keep, :);
    f = (keep(:) - 1) * fs / fftLength;
  elseif strcmpi(frequencyRange, 'twosided')
    f = (0:fftLength - 1)' * fs / fftLength;
  else
    negativeCount = floor((fftLength - 1) / 2);
    if mod(fftLength, 2) == 0
      order = [negativeCount + 3:fftLength, 1:negativeCount + 2];
    else
      order = [negativeCount + 2:fftLength, 1:negativeCount + 1];
    end
    s = s(order, :);
    f = (-negativeCount:fftLength - negativeCount - 1)' * fs / fftLength;
  end
  t = (starts(:) + length(window) / 2 - 1) / fs;
end
%=============================================================================
