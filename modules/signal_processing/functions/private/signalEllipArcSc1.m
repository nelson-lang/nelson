%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function z = signalEllipArcSc1(w, m)
  mc = 1 - m;
  lo = 0;
  hi = signalEllipke(mc) * (1 - 10 * eps());
  for idx = 1:80
    mid = (lo + hi) / 2;
    [s, c] = signalEllipj(mid, mc);
    if real(s / c) < w
      lo = mid;
    else
      hi = mid;
    end
  end
  z = (lo + hi) / 2;
end
%=============================================================================
