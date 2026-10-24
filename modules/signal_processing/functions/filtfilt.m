%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = filtfilt(b, a, x)
  narginchk(3, 3);
  nargoutchk(0, 1);
  b = b(:).';
  a = a(:).';
  if isempty(a) || isempty(b)
    error('Nelson:validators:mustBeNonempty', '%s must be nonempty.', 'Filter coefficients');
  end
  if a(1) == 0
    error('Nelson:signal:zeroLeadingDenominator', 'First denominator filter coefficient must be nonzero.');
  end
  b = b ./ a(1);
  a = a ./ a(1);
  if isvector(x)
    y = filtfiltVector(b, a, x);
    return;
  end
  y = zeros(size(x));
  for col = 1:size(x, 2)
    y(:, col) = filtfiltVector(b, a, x(:, col));
  end
end
%=============================================================================
function y = filtfiltVector(b, a, x)
  wasRow = size(x, 1) == 1;
  x = x(:);
  order = max(length(a), length(b)) - 1;
  if order == 0
    y = (b(1) ^ 2) * x;
    if wasRow
      y = y.';
    end
    return;
  end
  b = [b, zeros(1, order + 1 - length(b))];
  a = [a, zeros(1, order + 1 - length(a))];
  edge = min(3 * order, length(x) - 1);
  extended = oddExtension(x, edge);
  zi = steadyStateInitialConditions(b, a);
  y = filter(b, a, extended, zi * extended(1));
  y = flip(y);
  y = filter(b, a, y, zi * y(1));
  y = flip(y);
  y = y(edge + 1:edge + length(x));
  if wasRow
    y = y.';
  end
end
%=============================================================================
function extended = oddExtension(x, edge)
  if edge <= 0
    extended = x;
    return;
  end
  left = 2 * x(1) - x(edge + 1:-1:2);
  right = 2 * x(end) - x(end - 1:-1:end - edge);
  extended = [left; x; right];
end
%=============================================================================
function zi = steadyStateInitialConditions(b, a)
  order = length(a) - 1;
  dcgain = sum(b) / sum(a);
  zi = zeros(order, 1);
  zi(order) = b(order + 1) - a(order + 1) * dcgain;
  for k = order - 1:-1:1
    zi(k) = zi(k + 1) + b(k + 1) - a(k + 1) * dcgain;
  end
end
%=============================================================================
