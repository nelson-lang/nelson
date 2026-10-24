%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [zerosArray, poles] = signalCheby2Prototype(n, rs)
  epsilon = 1 / sqrt(10 ^ (rs / 10) - 1);
  mu = asinh(1 / epsilon) / n;
  zerosArray = [];
  poles = zeros(n, 1);
  for k = 1:n
    theta = pi * (2 * k - 1) / (2 * n);
    pole = -sinh(mu) * sin(theta) + 1i * cosh(mu) * cos(theta);
    poles(k) = 1 / pole;
    c = cos(theta);
    if abs(c) > eps()
      zerosArray = [zerosArray; 1i / c];
    end
  end
end
%=============================================================================
