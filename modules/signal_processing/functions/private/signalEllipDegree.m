%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function m = signalEllipDegree(n, m1)
  k1 = signalEllipke(m1);
  k1p = signalEllipke(1 - m1);
  q = exp(-pi * k1p / (n * k1));
  numerator = 0;
  denominator = 1;
  for idx = 0:7
    numerator = numerator + q ^ (idx * (idx + 1));
    denominator = denominator + 2 * q ^ ((idx + 1) ^ 2);
  end
  m = 16 * q * (numerator / denominator) ^ 4;
end
%=============================================================================
