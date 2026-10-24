%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = signalMedian(x)
  x = x(:);
  n = length(x);
  if n == 0
    y = NaN;
    return;
  end
  for i = 2:n
    value = x(i);
    j = i - 1;
    while j >= 1 && x(j) > value
      x(j + 1) = x(j);
      j = j - 1;
    end
    x(j + 1) = value;
  end
  if mod(n, 2) == 1
    y = x((n + 1) / 2);
  else
    y = (x(n / 2) + x(n / 2 + 1)) / 2;
  end
end
%=============================================================================
