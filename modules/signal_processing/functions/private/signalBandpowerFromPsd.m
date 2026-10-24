%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function p = signalBandpowerFromPsd(pxx, f, freqRange)
  f = f(:);
  if isempty(freqRange)
    width = signalPsdRectangleWidth(f, false);
    p = sum(pxx .* width, 1);
    return;
  end
  idx1 = find(f <= freqRange(1), 1, 'last');
  idx2 = find(f >= freqRange(2), 1, 'first');
  width = signalPsdRectangleWidth(f, true);
  p = sum(pxx(idx1:idx2, :) .* width(idx1:idx2), 1);
end
%=============================================================================
function width = signalPsdRectangleWidth(f, rangeSpecified)
  df = diff(f);
  if rangeSpecified
    width = [df; 0];
  else
    missingWidth = (f(end) - f(1)) / (length(f) - 1);
    if f(1) ~= 0
      width = [missingWidth; df];
    else
      width = [df; missingWidth];
    end
  end
end
%=============================================================================
