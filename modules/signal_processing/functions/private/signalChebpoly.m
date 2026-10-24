%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = signalChebpoly(n, x)
  y = zeros(size(x));
  inside = abs(x) <= 1;
  y(inside) = cos(n * acos(x(inside)));
  above = x > 1;
  y(above) = cosh(n * acosh(x(above)));
  below = x < -1;
  y(below) = ((-1) ^ n) * cosh(n * acosh(-x(below)));
end
%=============================================================================
