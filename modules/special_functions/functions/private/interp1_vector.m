%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function yq = interp1_vector(x, y, q, method, extrapMode, extrapValue)
  n = numel(x);
  yq = zeros(size(q)) + 0 * y(1);
  defaultExtrap = any(strcmp(method, {'pchip', 'makima', 'spline'}));

  if any(strcmp(method, {'pchip', 'makima', 'spline', 'cubic'}))
    d = interp1_pchip_slopes(x, y);
  else
    d = [];
  end

  for k = 1:numel(q)
    xk = q(k);
    if isnan(xk)
      yq(k) = NaN;
      continue
    end
    outside = (xk < x(1)) || (xk > x(end));
    if outside && strcmp(extrapMode, 'constant')
      yq(k) = extrapValue;
      continue
    end
    if outside && strcmp(extrapMode, 'default') && ~defaultExtrap
      yq(k) = NaN;
      continue
    end

    if xk <= x(1)
      left = 1;
    elseif xk >= x(end)
      left = n - 1;
    else
      left = find(x <= xk, 1, 'last');
      if left >= n
        left = n - 1;
      end
    end

    switch method
      case 'nearest'
        if abs(xk - x(left)) <= abs(x(left + 1) - xk)
          yq(k) = y(left);
        else
          yq(k) = y(left + 1);
        end
      case 'previous'
        if xk < x(1)
          yq(k) = y(1);
        else
          idx = find(x <= xk, 1, 'last');
          yq(k) = y(idx);
        end
      case 'next'
        if xk > x(end)
          yq(k) = y(end);
        else
          idx = find(x >= xk, 1, 'first');
          yq(k) = y(idx);
        end
      case {'pchip', 'makima', 'spline', 'cubic'}
        h = x(left + 1) - x(left);
        t = (xk - x(left)) / h;
        h00 = (2 * t^3) - (3 * t^2) + 1;
        h10 = (t^3) - (2 * t^2) + t;
        h01 = (-2 * t^3) + (3 * t^2);
        h11 = (t^3) - (t^2);
        yq(k) = h00 * y(left) + h10 * h * d(left) + h01 * y(left + 1) + h11 * h * d(left + 1);
      otherwise
        frac = (xk - x(left)) / (x(left + 1) - x(left));
        yq(k) = y(left) + frac * (y(left + 1) - y(left));
    end
  end
end
%=============================================================================
function d = interp1_pchip_slopes(x, y)
  n = numel(x);
  h = diff(x);
  delta = diff(y) ./ h;
  d = zeros(size(y)) + 0 * y(1);
  if n == 2
    d(:) = delta(1);
    return
  end
  for k = 2:n - 1
    if isreal(delta(k - 1)) && isreal(delta(k)) && delta(k - 1) * delta(k) > 0
      w1 = 2 * h(k) + h(k - 1);
      w2 = h(k) + 2 * h(k - 1);
      d(k) = (w1 + w2) / (w1 / delta(k - 1) + w2 / delta(k));
    else
      d(k) = (delta(k - 1) + delta(k)) / 2;
    end
  end
  d(1) = interp1_pchip_endpoint(h(1), h(2), delta(1), delta(2));
  d(n) = interp1_pchip_endpoint(h(end), h(end - 1), delta(end), delta(end - 1));
end
%=============================================================================
function d = interp1_pchip_endpoint(h1, h2, del1, del2)
  d = ((2 * h1 + h2) * del1 - h1 * del2) / (h1 + h2);
  if isreal(d) && isreal(del1) && isreal(del2)
    if sign(d) ~= sign(del1)
      d = 0;
    elseif sign(del1) ~= sign(del2) && abs(d) > abs(3 * del1)
      d = 3 * del1;
    end
  end
end
%=============================================================================
