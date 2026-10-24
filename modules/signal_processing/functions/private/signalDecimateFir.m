%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = signalDecimateFir(x, q, n)
  wasRow = size(x, 1) == 1;
  xv = x(:).';
  b = fir1(n, 1 / q);
  filtered = conv(xv, b);
  outputLength = ceil(length(xv) / q);
  delay = floor(n / 2);
  indices = delay + 1:q:delay + 1 + (outputLength - 1) * q;
  y = filtered(indices);
  if mod(n, 2) == 0
    y(1) = xv(1);
  end
  if ~wasRow
    y = y.';
  end
end
%=============================================================================
