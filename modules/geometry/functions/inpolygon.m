%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [in, on] = inpolygon(x, y, xv, yv)
  % points inside or on the edge of a polygon; NaN separates loops and the even-odd rule handles holes
  narginchk(4, 4);
  nargoutchk(0, 2);
  if ~isequal(size(x), size(y)) || numel(xv) ~= numel(yv)
    message = 'X and Y must have the same size, and XV and YV the same number of elements.';
    error('Nelson:inpolygon:sizeMismatch', message);
  end
  xq = x(:);
  yq = y(:);
  xv = xv(:);
  yv = yv(:);
  in = false(size(xq));
  on = false(size(xq));
  breaks = [0; find(isnan(xv) | isnan(yv)); numel(xv) + 1];
  for b = 1:numel(breaks) - 1
    loop = breaks(b) + 1:breaks(b + 1) - 1;
    if isempty(loop)
      continue
    end
    lx = xv(loop);
    ly = yv(loop);
    if lx(end) ~= lx(1) || ly(end) ~= ly(1)
      lx(end + 1) = lx(1);
      ly(end + 1) = ly(1);
    end
    for e = 1:numel(lx) - 1
      x1 = lx(e);
      y1 = ly(e);
      x2 = lx(e + 1);
      y2 = ly(e + 1);
      dx = x2 - x1;
      dy = y2 - y1;
      cross = dx * (yq - y1) - dy * (xq - x1);
      tolerance = 10 * eps(max([abs(x1), abs(x2), abs(y1), abs(y2), 1])) * max(abs(dx), abs(dy));
      inBox = xq >= min(x1, x2) & xq <= max(x1, x2) & yq >= min(y1, y2) & yq <= max(y1, y2);
      on = on | (inBox & abs(cross) <= tolerance);
      straddles = (y1 > yq) ~= (y2 > yq);
      crosses = straddles & (xq < dx * (yq - y1) ./ dy + x1);
      in = xor(in, crosses);
    end
  end
  in = reshape(in | on, size(x));
  on = reshape(on, size(x));
end
%=============================================================================
