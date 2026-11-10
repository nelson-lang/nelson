%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [Xr, Yr, xMap, yMap, pageSize] = pageBroadcast(X, Y)
  % page views of X and Y plus the page each output page reads after expansion
  if ~(isfloat(X) && isfloat(Y))
    error('Nelson:linalg:pageFloatInput', 'Inputs must be single or double arrays.');
  end
  sx = size(X);
  sy = size(Y);
  nd = max(numel(sx), numel(sy));
  sx(end + 1:nd) = 1;
  sy(end + 1:nd) = 1;
  px = sx(3:nd);
  py = sy(3:nd);
  if any(px ~= py & px ~= 1 & py ~= 1)
    error('Nelson:linalg:pageSizeMismatch', 'Dimensions beyond the second must match or be 1.');
  end
  pageSize = px;
  pageSize(px == 1) = py(px == 1);
  count = prod(pageSize);
  rest = (0:count - 1).';
  xMap = zeros(count, 1);
  yMap = zeros(count, 1);
  xStride = 1;
  yStride = 1;
  for d = 1:numel(pageSize)
    s = mod(rest, pageSize(d));
    rest = floor(rest / pageSize(d));
    xMap = xMap + min(s, px(d) - 1) * xStride;
    yMap = yMap + min(s, py(d) - 1) * yStride;
    xStride = xStride * px(d);
    yStride = yStride * py(d);
  end
  xMap = xMap + 1;
  yMap = yMap + 1;
  Xr = reshape(X, sx(1), sx(2), []);
  Yr = reshape(Y, sy(1), sy(2), []);
end
%=============================================================================
