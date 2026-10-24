%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [spectrum, f] = signalWelchCore(x, y, win, overlap, nfft, fs, range, spectrumType)
  isCross = ~isempty(y);
  x = x(:);
  if isCross
    y = y(:);
    n = min(length(x), length(y));
    x = x(1:n);
    y = y(1:n);
  end
  if isempty(win)
    win = hamming(min(256, length(x)));
  elseif isscalar(win)
    win = hamming(win);
  else
    win = win(:);
  end
  if isempty(overlap)
    overlap = floor(length(win) / 2);
  end
  if isempty(nfft)
    nfft = max(256, 2 ^ nextpow2(length(win)));
  end
  if overlap >= length(win)
    error('Nelson:signal:invalidOverlapLength', 'Overlap length must be less than window length.');
  end
  step = length(win) - overlap;
  starts = 1:step:(length(x) - length(win) + 1);
  if isempty(starts)
    error('Nelson:signal:signalTooShort', 'Signal length must be at least the window length.');
  end
  frameRows = (0:length(win) - 1).';
  indices = frameRows + starts;
  winMatrix = repmat(win, 1, length(starts));
  xFrames = x(indices) .* winMatrix;
  xPadded = zeros(nfft, length(starts));
  xPadded(1:length(win), :) = xFrames;
  xDft = fft(xPadded, nfft, 1);
  scale = fs * sum(abs(win) .^ 2);
  if isCross
    yFrames = y(indices) .* winMatrix;
    yPadded = zeros(nfft, length(starts));
    yPadded(1:length(win), :) = yFrames;
    rawSpectrum = sum(xDft .* conj(fft(yPadded, nfft, 1)), 2) ./ (scale * length(starts));
    useRealSymmetry = isreal(x) && isreal(y);
  else
    rawSpectrum = sum(abs(xDft) .^ 2, 2) ./ (scale * length(starts));
    if strcmp(spectrumType, 'power')
      rawSpectrum = rawSpectrum .* scale ./ (abs(sum(win)) .^ 2);
    end
    useRealSymmetry = isreal(x);
  end
  [spectrum, f] = signalSpectrumRange(rawSpectrum, nfft, fs, range, useRealSymmetry);
end
%=============================================================================
