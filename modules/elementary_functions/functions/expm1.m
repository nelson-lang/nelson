%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = expm1(x)
  narginchk(1, 1);
  nargoutchk(0, 1);
  if ~isreal(x)
    y = exp(x) - 1;
    return
  end
  % Kahan's algorithm: accurate for small x
  u = exp(x);
  y = u - 1;
  m = (u ~= 1) & (y ~= -1) & isfinite(u);
  y(m) = y(m) .* x(m) ./ log(u(m));
  m = (u == 1);
  y(m) = x(m);
end
%=============================================================================
