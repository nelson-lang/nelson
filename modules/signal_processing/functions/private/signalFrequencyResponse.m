%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function h = signalFrequencyResponse(b, a, w)
  imaginaryUnit = complex(0, 1);
  b = b(:).';
  a = a(:).';
  kB = 0:length(b) - 1;
  kA = 0:length(a) - 1;
  originalShape = size(w);
  w = w(:);
  h = zeros(size(w));
  for idx = 1:length(w)
    numerator = sum(b .* exp(-imaginaryUnit * w(idx) * kB));
    denominator = sum(a .* exp(-imaginaryUnit * w(idx) * kA));
    h(idx) = numerator ./ denominator;
  end
  h = reshape(h, originalShape);
end
%=============================================================================
