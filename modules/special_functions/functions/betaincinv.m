%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function X = betaincinv(Y, Z, W, tail)
  % inverse of the regularized incomplete beta function with respect to x
  narginchk(3, 4);
  upper = false;
  if nargin > 3
    upper = parseTail(tail, 'Nelson:betaincinv:invalidTail');
  end
  [Y, Z, W] = checkInverseInputs(Y, Z, W, 'betaincinv');
  if any(Z(:) < 0) || any(W(:) < 0)
    error('Nelson:betaincinv:negativeParameters', 'Z and W must be nonnegative.');
  end
  X = NaN(size(Y), 'like', Y);
  lowerEdge = ((Y == 0 & ~upper) | (Y == 1 & upper) | Z == 0) & ~isnan(Y);
  upperEdge = ((Y == 1 & ~upper) | (Y == 0 & upper) | (W == 0 & Z > 0)) & ~isnan(Y);
  X(upperEdge) = 1;
  X(lowerEdge) = 0;
  work = ~(lowerEdge | upperEdge | isnan(Y) | isnan(Z) | isnan(W) | isinf(Z) | isinf(W));
  if ~any(work(:))
    return
  end
  y = Y(work);
  y = y(:);
  z = Z(work);
  z = z(:);
  w = W(work);
  w = w(:);
  if upper
    q = y;
    p = 1 - q;
    zscore = sqrt(2) * erfcinv(2 * q);
  else
    p = y;
    q = 1 - p;
    zscore = -sqrt(2) * erfcinv(2 * p);
  end
  logBeta = betaln(z, w);
  residual = @(u, k) betaResidual(u, z(k), w(k), y(k), upper);
  slope = @(u, k) exp(-z(k) .* log1p(exp(-u)) - w(k) .* log1p(exp(u)) - logBeta(k));
  logSmall = (log(p) + log(z) + logBeta) ./ z;
  smallGuess = logSmall - log(-expm1(min(logSmall, -eps)));
  logLarge = (log(q) + log(w) + logBeta) ./ w;
  largeGuess = log(-expm1(min(logLarge, -eps))) - logLarge;
  m = z ./ (z + w);
  sd = sqrt(z .* w ./ ((z + w) .^ 2 .* (z + w + 1)));
  xm = min(max(m + zscore .* sd, 1e-3), 1 - 1e-3);
  normalGuess = log(xm ./ (1 - xm));
  u = bestGuess(residual, smallGuess, largeGuess, normalGuess);
  u = safeNewton(residual, slope, u, -745, 745);
  X(work) = 1 ./ (1 + exp(-u));
end
%=============================================================================
function r = betaResidual(u, z, w, y, upper)
  % residual that increases with the logit u of x
  x = 1 ./ (1 + exp(-u));
  if upper
    r = y - betainc(x, z, w, 'upper');
  else
    r = betainc(x, z, w) - y;
  end
end
%=============================================================================
