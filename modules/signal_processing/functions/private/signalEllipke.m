%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function k = signalEllipke(m)
  a = 1;
  b = sqrt(1 - m);
  for idx = 1:32
    nextA = (a + b) / 2;
    b = sqrt(a * b);
    a = nextA;
    if abs(a - b) <= eps() * max(1, abs(a))
      break;
    end
  end
  k = pi / (2 * a);
end
%=============================================================================
