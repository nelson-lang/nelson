%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = spline(x, y, xq)
  % not-a-knot cubic spline values on the device: node slopes from one tridiagonal solve, then a
  % cubic Hermite piece per query; other forms run on the host
  if nargin == 3 && isvector(x) && isvector(y) && numel(x) == numel(y) && numel(x) >= 4 ...
      && isfloat(x) && isfloat(y) && isfloat(xq) && isreal(x) && isreal(y) && isreal(xq)
    x = gpuArray(x(:));
    y = gpuArray(y(:));
    n = numel(x);
    h = diff(x);
    if gather(all(h > 0))
      d = diff(y) ./ h;
      % interior rows: h(i) s(i-1) + 2 (h(i-1) + h(i)) s(i) + h(i-1) s(i+1) = 3 (h(i) d(i-1) + h(i-1) d(i))
      main = [h(2); 2 * (h(1:n - 2) + h(2:n - 1)); h(n - 2)];
      upper = [h(1) + h(2); h(1:n - 2)];
      lower = [h(2:n - 1); h(n - 1) + h(n - 2)];
      rhs = [((h(1) + 2 * (h(1) + h(2))) * h(2) * d(1) + h(1) ^ 2 * d(2)) / (h(1) + h(2)); ...
             3 * (h(2:n - 1) .* d(1:n - 2) + h(1:n - 2) .* d(2:n - 1)); ...
             (h(n - 1) ^ 2 * d(n - 2) + (2 * (h(n - 2) + h(n - 1)) + h(n - 1)) * h(n - 2) * d(n - 1)) / (h(n - 2) + h(n - 1))];
      A = diag(main) + diag(upper, 1) + diag(lower, -1);
      s = A \ rhs;
      % interval of every query; the end pieces extend outside the data
      k = discretize(xq(:), x);
      k(xq(:) < x(1)) = 1;
      k(xq(:) > x(n)) = n - 1;
      k(isnan(k)) = 1;
      t = xq(:) - x(k);
      hk = h(k);
      dk = d(k);
      s0 = s(k);
      s1 = s(k + 1);
      c2 = (3 * dk - 2 * s0 - s1) ./ hk;
      c3 = (s0 + s1 - 2 * dk) ./ hk .^ 2;
      v = y(k) + t .* (s0 + t .* (c2 + t .* c3));
      v(isnan(xq(:))) = NaN;
      out = reshape(v, size(xq));
      return
    end
  end
  out = __gpuArray_host__('spline', x, y, xq);
end
%=============================================================================
