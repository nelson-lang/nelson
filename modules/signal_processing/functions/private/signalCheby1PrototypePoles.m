%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function poles = signalCheby1PrototypePoles(n, rp)
  epsilon = sqrt(10 ^ (rp / 10) - 1);
  mu = asinh(1 / epsilon) / n;
  poles = zeros(n, 1);
  for k = 1:n
    theta = pi * (2 * k - 1) / (2 * n);
    poles(k) = -sinh(mu) * sin(theta) + 1i * cosh(mu) * cos(theta);
  end
end
%=============================================================================
