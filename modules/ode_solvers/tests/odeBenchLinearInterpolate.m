%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [y, yp] = odeBenchLinearInterpolate(t, values, x, slopes)
  exact = find(t == x, 1);
  if ~isempty(exact)
    y = values(:, exact);
    yp = slopes(:, exact);
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
  dt = t(idx + 1) - t(idx);
  alpha = (x - t(idx)) / dt;
  [y, yp] = odeBenchHermite(values(:, idx), values(:, idx + 1), ...
    slopes(:, idx), slopes(:, idx + 1), dt, alpha);
end
%=============================================================================
function [y, yp] = odeBenchHermite(y0, y1, yp0, yp1, dt, alpha)
  h00 = 2 * alpha ^ 3 - 3 * alpha ^ 2 + 1;
  h10 = alpha ^ 3 - 2 * alpha ^ 2 + alpha;
  h01 = -2 * alpha ^ 3 + 3 * alpha ^ 2;
  h11 = alpha ^ 3 - alpha ^ 2;
  y = h00 * y0 + h10 * dt * yp0 + h01 * y1 + h11 * dt * yp1;
  dh00 = 6 * alpha ^ 2 - 6 * alpha;
  dh10 = 3 * alpha ^ 2 - 4 * alpha + 1;
  dh01 = -6 * alpha ^ 2 + 6 * alpha;
  dh11 = 3 * alpha ^ 2 - 2 * alpha;
  yp = (dh00 * y0 + dh10 * dt * yp0 + dh01 * y1 + dh11 * dt * yp1) / dt;
end
%=============================================================================
