%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [pxx, f] = signalPeriodogramCore(x, win, nfft, fs, range, spectrumType)
  if nargin < 5
    range = '';
  end
  if nargin < 6
    spectrumType = 'psd';
  end
  x = x(:);
  if isempty(win)
    win = ones(length(x), 1);
  elseif isscalar(win)
    win = rectwin(win);
  else
    win = win(:);
  end
  x = x(1:min(length(x), length(win)));
  win = win(1:length(x));
  if isempty(nfft)
    nfft = max(256, 2 ^ nextpow2(length(x)));
  end
  xw = x .* win;
  padded = zeros(nfft, 1);
  padded(1:length(xw)) = xw;
  xdft = fft(padded);
  scale = fs * sum(abs(win) .^ 2);
  rawPxx = abs(xdft) .^ 2 ./ scale;
  if strcmp(spectrumType, 'power')
    rawPxx = rawPxx .* scale ./ (abs(sum(win)) .^ 2);
  end
  [pxx, f] = signalSpectrumRange(rawPxx, nfft, fs, range, isreal(x));
end
%=============================================================================
