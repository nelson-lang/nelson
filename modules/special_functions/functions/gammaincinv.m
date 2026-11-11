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
  % solve on the smaller tail, its complement 1 - y is exact
  useUpper = q < p;
  smallGuess = (log(p) + gammaln(a + 1)) ./ a;
  w = 1 - 1 ./ (9 * a) + zscore ./ (3 * sqrt(a));
  normalGuess = log(a .* max(w, 1e-3) .^ 3);
  tailGuess = log(max(-log(q) - logGammaA, a));
  logTarget = log(min(p, q));
  residual = @(t, k) gammaResidual(t, a(k), logTarget(k), logGammaA(k), useUpper(k));
  t = bestGuess(residual, smallGuess, normalGuess, tailGuess);
  x = exp(safeNewton(residual, [], t, -745, 709));
  X(work) = x;
end
%=============================================================================
function rs = gammaResidual(t, a, logTarget, logGammaA, useUpper)
  % log residual that increases with x = exp(t), and its slope in t, as two columns
  x = exp(t);
  if ~any(useUpper)
    logTail = log(gammainc(x, a));
  elseif all(useUpper)
    logTail = log(gammainc(x, a, 'upper'));
  else
    % products by 0 and 1 are exact
    logTail = log(gammainc(x, a) .* ~useUpper + gammainc(x, a, 'upper') .* useUpper);
  end
  r = (logTail - logTarget) .* (1 - 2 * useUpper);
  rs = [r, exp(-x + a .* t - logGammaA - logTail)];
end
%=============================================================================
