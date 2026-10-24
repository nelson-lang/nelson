%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Change of variable from the normalized variable T in [-1, 1] to x in
% [a, b] (finite or infinite): abscissae X and jacobian W.
%=============================================================================
function [X, W] = integral_map(kind, a, b, T)
  switch kind
    case 'finite'
      A = 0.5 * (b - a);
      B = 0.5 * (a + b);
      X = A .* (3 * T - T .^ 3) / 2 + B;
      W = A .* (3 - 3 * T .^ 2) / 2;
    case 'ainf'
      X = a + (1 + T) ./ (1 - T);
      W = 2 ./ (1 - T) .^ 2;
    case 'binf'
      X = b - (1 - T) ./ (1 + T);
      W = 2 ./ (1 + T) .^ 2;
    otherwise
      X = T ./ (1 - T .^ 2);
      W = (1 + T .^ 2) ./ (1 - T .^ 2) .^ 2;
  end
end
%=============================================================================
