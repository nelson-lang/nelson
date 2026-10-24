%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [fmean, pwr] = signalMeanfreqFromPsd(pxx, f, freqRange)
  f = f(:);
  if isempty(freqRange)
    freqRange = [f(1), f(end)];
  end
  width = signalFrequencyBinWidth(f);
  power = pxx .* width;
  idx = f >= freqRange(1) & f <= freqRange(2);
  selectedPower = power(idx, :);
  selectedFreq = f(idx);
  pwr = sum(selectedPower, 1);
  fmean = sum(selectedPower .* selectedFreq, 1) ./ pwr;
end
%=============================================================================
