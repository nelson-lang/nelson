%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = thd(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  x = varargin{1}(:);
  X = abs(fft(x));
  X = X(1:floor(length(X) / 2));
  [fundamental, idx] = max(X(2:end));
  idx = idx + 1;
  fundamentalOffset = idx - 1;
  harmonicPower = 0;
  harmonic = 2;
  harmonicIndex = 1 + harmonic * fundamentalOffset;
  while harmonicIndex <= length(X)
    harmonicPower = harmonicPower + X(harmonicIndex) ^ 2;
    harmonic = harmonic + 1;
    harmonicIndex = 1 + harmonic * fundamentalOffset;
  end
  value = 10 * log10(harmonicPower / (fundamental ^ 2));
end
%=============================================================================
