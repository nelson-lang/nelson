%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function pp = interp1_pp(x, v, method)
  x = full(x(:)).';
  if ~isvector(v)
    error(_('PP form is supported for vector sample values.'));
  end
  v = full(v(:)).';
  if numel(x) ~= numel(v)
    error(_('X and V must be of the same length.'));
  end
  if strcmp(method, 'nearest') || strcmp(method, 'previous') || strcmp(method, 'next')
    error(_('PP form is supported for continuous interpolation methods.'));
  end
  if any(diff(x) < 0)
    x = fliplr(x);
    v = fliplr(v);
  end
  n = numel(x);
  coefs = zeros(n - 1, 4) + 0 * v(1);
  if strcmp(method, 'linear')
    for k = 1:n - 1
      slope = (v(k + 1) - v(k)) / (x(k + 1) - x(k));
      coefs(k, :) = [0, 0, slope, v(k)];
    end
  else
    d = interp1_pchip_slopes_local(x(:), v(:));
    for k = 1:n - 1
      h = x(k + 1) - x(k);
      c0 = v(k);
      c1 = d(k);
      c2 = (3 * (v(k + 1) - v(k)) / h - 2 * d(k) - d(k + 1)) / h;
      c3 = (2 * (v(k) - v(k + 1)) / h + d(k) + d(k + 1)) / (h * h);
      coefs(k, :) = [c3, c2, c1, c0];
    end
  end
  pp = struct('form', 'pp', 'breaks', x, 'coefs', coefs, 'pieces', n - 1, 'order', 4, 'dim', 1);
end
%=============================================================================
function d = interp1_pchip_slopes_local(x, y)
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
  d(1) = ((2 * h(1) + h(2)) * delta(1) - h(1) * delta(2)) / (h(1) + h(2));
  d(n) = ((2 * h(end) + h(end - 1)) * delta(end) - h(end) * delta(end - 1)) / (h(end) + h(end - 1));
end
%=============================================================================
