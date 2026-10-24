%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [y, yp] = odeLinearInterpolate(t, values, x, slopes)
  if nargin < 4
    slopes = [];
  end
  exact = find(t == x, 1);
  if ~isempty(exact)
    y = values(:, exact);
    if ~isempty(slopes) && size(slopes, 2) >= exact
      yp = slopes(:, exact);
    else
      yp = zeros(size(y));
    end
    return
  end
  forward = t(length(t)) >= t(1);
  if forward && x <= t(1)
    idx = 1;
  elseif forward && x >= t(length(t))
    idx = length(t) - 1;
  elseif ~forward && x >= t(1)
    idx = 1;
  elseif ~forward && x <= t(length(t))
    idx = length(t) - 1;
  else
    idx = find((t(1:end - 1) <= x & x <= t(2:end)) ...
      | (t(1:end - 1) >= x & x >= t(2:end)), 1);
  end
  if isempty(idx)
    idx = max(1, length(t) - 1);
  end
  dt = t(idx + 1) - t(idx);
  if dt == 0
    alpha = 0;
    yp = zeros(size(values, 1), 1);
  else
    alpha = (x - t(idx)) / dt;
    yp = (values(:, idx + 1) - values(:, idx)) / dt;
  end
  if ~isempty(slopes) && size(slopes, 2) >= idx + 1
    [y, yp] = odeHermite(values(:, idx), values(:, idx + 1), ...
      slopes(:, idx), slopes(:, idx + 1), dt, alpha);
    return
  end
  y = values(:, idx) + alpha * (values(:, idx + 1) - values(:, idx));
end
%=============================================================================
function [y, yp] = odeHermite(y0, y1, yp0, yp1, dt, alpha)
  h00 = 2 * alpha ^ 3 - 3 * alpha ^ 2 + 1;
  h10 = alpha ^ 3 - 2 * alpha ^ 2 + alpha;
  h01 = -2 * alpha ^ 3 + 3 * alpha ^ 2;
  h11 = alpha ^ 3 - alpha ^ 2;
  y = h00 * y0 + h10 * dt * yp0 + h01 * y1 + h11 * dt * yp1;
  if dt == 0
    yp = zeros(size(y0));
    return
  end
  dh00 = 6 * alpha ^ 2 - 6 * alpha;
  dh10 = 3 * alpha ^ 2 - 4 * alpha + 1;
  dh01 = -6 * alpha ^ 2 + 6 * alpha;
  dh11 = 3 * alpha ^ 2 - 2 * alpha;
  yp = (dh00 * y0 + dh10 * dt * yp0 + dh01 * y1 + dh11 * dt * yp1) / dt;
end
%=============================================================================
