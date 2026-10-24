%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function h = signalIntfilt(r, n, alpha)
  order = 2 * r * n - 2;
  halfOrder = order / 2;
  [f, m] = interpolationBands(r, alpha);
  gram = zeros(halfOrder + 1, halfOrder + 1);
  rhs = zeros(halfOrder + 1, 1);
  for bandIndex = 1:2:length(f)
    a = f(bandIndex);
    b = f(bandIndex + 1);
    bandGain = m(bandIndex);
    for row = 0:halfOrder
      rhs(row + 1) = rhs(row + 1) + bandGain * cosineIntegral(row, a, b);
      for col = 0:halfOrder
        gram(row + 1, col + 1) = gram(row + 1, col + 1) + cosineProductIntegral(row, col, a, b);
      end
    end
  end
  coeffs = gram \ rhs;
  h = zeros(1, order + 1);
  center = halfOrder + 1;
  h(center) = coeffs(1);
  for k = 1:halfOrder
    value = coeffs(k + 1) / 2;
    h(center - k) = value;
    h(center + k) = value;
  end
end
%=============================================================================
function [f, m] = interpolationBands(r, alpha)
  if alpha == 1
    f = [0, 1 / r, 1 / r, 1];
    m = [r, r, 0, 0];
    return;
  end
  f = [0, alpha / r];
  m = [r, r];
  imageCenter = 1 / r;
  while imageCenter <= 0.5 + eps
    f = [f, 2 * (imageCenter - alpha / (2 * r)), 2 * (imageCenter + alpha / (2 * r))];
    m = [m, 0, 0];
    imageCenter = imageCenter + 1 / r;
  end
  if f(end) > 1
    f(end) = 1;
  end
end
%=============================================================================
function value = cosineProductIntegral(i, j, a, b)
  if i == 0 && j == 0
    value = b - a;
  elseif i == 0
    value = cosineIntegral(j, a, b);
  elseif j == 0
    value = cosineIntegral(i, a, b);
  else
    value = (cosineIntegral(abs(i - j), a, b) + cosineIntegral(i + j, a, b)) / 2;
  end
end
%=============================================================================
function value = cosineIntegral(k, a, b)
  if k == 0
    value = b - a;
  else
    value = (sin(k * pi * b) - sin(k * pi * a)) / (k * pi);
  end
end
%=============================================================================
