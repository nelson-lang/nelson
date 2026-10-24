%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = signalDecimateIir(x, q, n)
  if length(x) <= 3 * n
    error('Nelson:signal:decimate:dataTooShort', 'Data length must be greater than three times the filter order.');
  end
  [b, a] = cheby1(n, 0.05, 0.8 / q);
  y = downsample(filtfilt(b, a, x), q, q - 1);
end
%=============================================================================
