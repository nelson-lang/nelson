%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [s, f, t] = signalSpectrogramCore(x, window, overlap, nfft, fs)
  step = length(window) - overlap;
  starts = 1:step:(length(x) - length(window) + 1);
  frames = zeros(length(window), length(starts));
  for idx = 1:length(starts)
    frames(:, idx) = x(starts(idx):starts(idx) + length(window) - 1) .* window;
  end
  padded = zeros(nfft, length(starts));
  padded(1:size(frames, 1), :) = frames;
  s = fft(padded, nfft, 1);
  keep = 1:floor(nfft / 2) + 1;
  s = s(keep, :);
  f = (keep(:) - 1) * fs / nfft;
  t = (starts(:) + length(window) / 2 - 1) / fs;
end
%=============================================================================
