%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [fm, pwr] = signalMedfreqFromPsd(pxx, f, freqRange)
  f = f(:);
  if isempty(freqRange)
    freqRange = [f(1), f(end)];
  end
  width = signalFrequencyBinWidth(f);
  cumulativeFrequency = [f(1); (f(1:end - 1) + f(2:end)) / 2; f(end)];
  fm = zeros(1, size(pxx, 2));
  pwr = zeros(1, size(pxx, 2));
  for col = 1:size(pxx, 2)
    power = width .* pxx(:, col);
    cumulativePower = [0; cumsum(power)];
    lowPower = interp1(cumulativeFrequency, cumulativePower, freqRange(1), 'linear');
    highPower = interp1(cumulativeFrequency, cumulativePower, freqRange(2), 'linear');
    pwr(col) = highPower - lowPower;
    fm(col) = interp1(cumulativePower, cumulativeFrequency, (lowPower + highPower) / 2, 'linear');
  end
end
%=============================================================================
