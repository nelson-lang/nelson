%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, stdx, mse, S] = lscov(A, b, V, alg)
  % least squares with known weights or covariance V, plus standard errors and mean squared error
  narginchk(2, 4);
  nargoutchk(0, 4);
  if ~(isfloat(A) && isfloat(b))
    error('Nelson:linalg:pageFloatInput', 'Inputs must be single or double arrays.');
  end
  [m, n] = size(A);
  if size(b, 1) ~= m
    error('Nelson:linalg:pageRowsMismatch', 'The two inputs must have the same number of rows.');
  end
  if nargin > 3 && ~((ischar(alg) || isstring(alg)) && any(strcmpi(char(alg), {'chol', 'orth'})))
    error('Nelson:lscov:invalidAlgorithm', 'Algorithm must be ''chol'' or ''orth''.');
  end
  A = full(A);
  b = full(b);
  C = zeros(0, n, 'like', A);
  c = zeros(0, size(b, 2), 'like', b);
  if nargin < 3 || isempty(V)
    Aw = A;
    bw = b;
  elseif isvector(V) && numel(V) == m && ~(m > 1 && isequal(size(V), [m, m]))
    if ~isreal(V) || any(V(:) < 0)
      invalidCovarianceError();
    end
    s = sqrt(V(:));
    Aw = A .* s;
    bw = b .* s;
  elseif isequal(size(V), [m, m])
    [T, failed] = chol(V);
    if failed == 0
      Aw = T' \ A;
      bw = T' \ b;
    else
      [E, D] = eig((V + V') / 2);
      d = real(diag(D));
      tolV = max(abs(d)) * m * eps(underlyingType(d));
      if any(d < -tolV)
        invalidCovarianceError();
      end
      kept = d > tolV;
      s = 1 ./ sqrt(d(kept));
      Aw = (E(:, kept)' * A) .* s;
      bw = (E(:, kept)' * b) .* s;
      C = E(:, ~kept)' * A;
      c = E(:, ~kept)' * b;
    end
  else
    invalidCovarianceError();
  end
  if isempty(C)
    [x, covariance, dfe] = solveWeighted(Aw, bw);
  else
    x0 = pinv(C) * c;
    N = null(C);
    [z, covarianceZ, dfe] = solveWeighted(Aw * N, bw - Aw * x0);
    x = x0 + N * z;
    covariance = N * covarianceZ * N';
  end
  if nargout < 2
    return
  end
  residual = bw - Aw * x;
  if dfe > 0
    mse = sum(abs(residual) .^ 2, 1) / dfe;
  else
    mse = zeros(1, size(b, 2), 'like', real(x));
  end
  stdx = sqrt(real(diag(covariance)) * mse);
  if nargout > 3
    if size(b, 2) ~= 1
      error('Nelson:lscov:covarianceSingleColumn', 'The covariance output needs a single right-hand side column.');
    end
    S = covariance * mse;
  end
end
%=============================================================================
function [x, covariance, dfe] = solveWeighted(Aw, bw)
  % basic least-squares solution with a pivoted QR; dependent columns get zero
  [m, n] = size(Aw);
  x = zeros(n, size(bw, 2), 'like', Aw(1) * bw(1));
  covariance = zeros(n, n, 'like', x);
  dfe = m;
  if n == 0 || m == 0
    return
  end
  [Q, R, perm] = qr(Aw, 0);
  d = abs(diag(R));
  r = sum(d > max(m, n) * eps(underlyingType(d)) * d(1));
  if r < n
    warning('Nelson:lscov:rankDeficient', 'The matrix is rank deficient: some coefficients are set to zero.');
  end
  dfe = m - r;
  if r == 0
    return
  end
  kept = perm(1:r);
  R1 = R(1:r, 1:r);
  x(kept, :) = R1 \ (Q(:, 1:r)' * bw);
  Ri = R1 \ eye(r, 'like', R1);
  covariance(kept, kept) = Ri * Ri';
end
%=============================================================================
function invalidCovarianceError()
  message = 'V must be an m-by-m covariance matrix or a vector of m nonnegative weights.';
  error('Nelson:lscov:invalidCovariance', message);
end
%=============================================================================
