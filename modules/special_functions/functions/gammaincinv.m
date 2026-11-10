%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function X = gammaincinv(Y, A, tail)
  % inverse of the regularized incomplete gamma function with respect to x
  narginchk(2, 3);
  upper = false;
  if nargin > 2
    upper = parseTail(tail, 'Nelson:gammaincinv:invalidTail');
  end
  [Y, A] = checkInverseInputs(Y, A, 'gammaincinv');
  if any(A(:) < 0)
    error('Nelson:gammaincinv:negativeA', 'A must be nonnegative.');
  end
  X = NaN(size(Y), 'like', Y);
  lowerEdge = (Y == 0 & ~upper) | (Y == 1 & upper) | (A == 0 & ~isnan(Y));
  upperEdge = ((Y == 1 & ~upper) | (Y == 0 & upper)) & A > 0;
  X(lowerEdge) = 0;
  X(upperEdge) = Inf;
  work = ~(lowerEdge | upperEdge | isnan(Y) | isnan(A) | isinf(A));
  if ~any(work(:))
    return
  end
  y = Y(work);
  y = y(:);
  a = A(work);
  a = a(:);
  if upper
    q = y;
    p = 1 - q;
    zscore = sqrt(2) * erfcinv(2 * q);
  else
    p = y;
    q = 1 - p;
    zscore = -sqrt(2) * erfcinv(2 * p);
  end
  logGammaA = gammaln(a);
  residual = @(t, k) gammaResidual(exp(t), a(k), y(k), upper);
  slope = @(t, k) exp(-exp(t) + a(k) .* t - logGammaA(k));
  smallGuess = (log(p) + gammaln(a + 1)) ./ a;
  w = 1 - 1 ./ (9 * a) + zscore ./ (3 * sqrt(a));
  normalGuess = log(a .* max(w, 1e-3) .^ 3);
  tailGuess = log(max(-log(q) - logGammaA, a));
  t = bestGuess(residual, smallGuess, normalGuess, tailGuess);
  X(work) = exp(safeNewton(residual, slope, t, -745, 709));
end
%=============================================================================
function r = gammaResidual(x, a, y, upper)
  % residual that increases with x
  if upper
    r = y - gammainc(x, a, 'upper');
  else
    r = gammainc(x, a) - y;
  end
end
%=============================================================================
