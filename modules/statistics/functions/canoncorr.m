%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [A, B, r, U, V, stats] = canoncorr(X, Y)
  narginchk(2, 2);
  nargoutchk(0, 6);
  [X, Y] = validateInputsLocal(X, Y);
  n = size(X, 1);
  X = X - repmat(mean(X, 1), n, 1);
  Y = Y - repmat(mean(Y, 1), n, 1);
  [Xr, keepX] = independentColumnsLocal(X, 'X');
  [Yr, keepY] = independentColumnsLocal(Y, 'Y');
  [Ax, Bx, r, Ux, Vx] = reducedCanoncorrLocal(Xr, Yr, n);
  A = zeros(size(X, 2), size(Ax, 2));
  B = zeros(size(Y, 2), size(Bx, 2));
  if isempty(r)
    U = zeros(n, 0);
    V = zeros(n, 0);
    if nargout > 5
      stats = statsLocal(r, n, sum(keepX), sum(keepY));
    end
    return
  end
  A(keepX, :) = Ax;
  B(keepY, :) = Bx;
  [A, B, U, V] = orientColumnsLocal(A, B, X, Y);
  if nargout > 5
    stats = statsLocal(r, n, sum(keepX), sum(keepY));
  end
end
%=============================================================================
function [X, Y] = validateInputsLocal(X, Y)
  if ~(isnumeric(X) && isreal(X) && ~issparse(X) && ndims(X) == 2)
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  if ~(isnumeric(Y) && isreal(Y) && ~issparse(Y) && ndims(Y) == 2)
    error('Nelson:statistics:yRealNumericMatrix', '%s must be a real numeric matrix.', 'Y');
  end
  if size(X, 1) ~= size(Y, 1)
    error('Nelson:statistics:xYSameNumberRows', '%s must have the same number of rows.', 'X and Y');
  end
  if size(X, 1) < 2 || size(X, 2) == 0 || size(Y, 2) == 0
    error('Nelson:statistics:xYContainLeastTwo', 'X and Y must contain at least two observations and one variable.');
  end
  X = double(X);
  Y = double(Y);
  if any(~isfinite(X(:))) || any(~isfinite(Y(:)))
    error('Nelson:statistics:xYContainFiniteValues', '%s must contain finite values.', 'X and Y');
  end
end
%=============================================================================
function [Xr, keep] = independentColumnsLocal(X, name)
  s = svd(X, 'econ');
  tolerance = max(size(X)) * eps(max([1; s(:)]));
  targetRank = sum(s > tolerance);
  keep = false(1, size(X, 2));
  Q = zeros(size(X, 1), 0);
  for k = 1:size(X, 2)
    v = X(:, k);
    if ~isempty(Q)
      v = v - Q * (Q' * v);
    end
    vnorm = norm(v);
    if vnorm > tolerance
      keep(k) = true;
      Q(:, end + 1) = v / vnorm;
    end
    if sum(keep) == targetRank
      break
    end
  end
  if sum(keep) < size(X, 2)
    warning(['Nelson:statistics:canoncorr:', name, 'RankDeficient'], ...
      sprintf(_('%s is not full rank.'), name));
  end
  Xr = X(:, keep);
end
%=============================================================================
function [A, B, r, U, V] = reducedCanoncorrLocal(X, Y, n)
  rx = size(X, 2);
  ry = size(Y, 2);
  d = min(rx, ry);
  if d == 0
    A = zeros(rx, 0);
    B = zeros(ry, 0);
    r = zeros(1, 0);
    U = zeros(n, 0);
    V = zeros(n, 0);
    return
  end
  [UX, SX, VX] = svd(X, 'econ');
  [UY, SY, VY] = svd(Y, 'econ');
  sx = diag(SX);
  sy = diag(SY);
  [L, D, M] = svd(UX' * UY, 'econ');
  r = min(max(diag(D(1:d, 1:d))', 0), 1);
  scale = sqrt(n - 1);
  A = VX(:, 1:rx) * (diag(scale ./ sx(1:rx)) * L(:, 1:d));
  B = VY(:, 1:ry) * (diag(scale ./ sy(1:ry)) * M(:, 1:d));
  U = X * A;
  V = Y * B;
end
%=============================================================================
function [A, B, U, V] = orientColumnsLocal(A, B, X, Y)
  U = X * A;
  V = Y * B;
  for k = 1:size(A, 2)
    if corrLocal(U(:, k), V(:, k)) < 0
      B(:, k) = -B(:, k);
      V(:, k) = -V(:, k);
    end
    [~, idx] = max(abs(A(:, k)));
    if A(idx, k) < 0
      A(:, k) = -A(:, k);
      B(:, k) = -B(:, k);
      U(:, k) = -U(:, k);
      V(:, k) = -V(:, k);
    end
  end
end
%=============================================================================
function c = corrLocal(x, y)
  denominator = norm(x) * norm(y);
  if denominator == 0
    c = NaN;
  else
    c = (x' * y) / denominator;
  end
end
%=============================================================================
function stats = statsLocal(r, n, p, q)
  d = length(r);
  Wilks = zeros(1, d);
  df1 = zeros(1, d);
  df2 = zeros(1, d);
  F = zeros(1, d);
  pF = zeros(1, d);
  chisq = zeros(1, d);
  pChisq = zeros(1, d);
  for k = 1:d
    pk = p - k + 1;
    qk = q - k + 1;
    lambda = prod(max(0, 1 - r(k:d) .^ 2));
    Wilks(k) = lambda;
    df1(k) = pk * qk;
    m = n - 1 - (pk + qk + 1) / 2;
    chisq(k) = max(0, -m * log(max(lambda, realmin)));
    pChisq(k) = chiSquareUpperLocal(chisq(k), df1(k));
    [F(k), df2(k), pF(k)] = raoFLocal(lambda, pk, qk, m);
  end
  stats = struct('Wilks', Wilks, 'df1', df1, 'df2', df2, 'F', F, ...
    'pF', pF, 'chisq', chisq, 'pChisq', pChisq, 'dfe', df1, 'p', pChisq);
end
%=============================================================================
function [F, df2, p] = raoFLocal(lambda, p, q, m)
  df1 = p * q;
  denom = p ^ 2 + q ^ 2 - 5;
  if denom > 0
    s = sqrt(max((p ^ 2 * q ^ 2 - 4) / denom, 1));
  else
    s = 1;
  end
  df2 = m * s - df1 / 2 + 1;
  lambdaRoot = max(lambda, realmin) .^ (1 / s);
  F = ((1 - lambdaRoot) / lambdaRoot) * (df2 / df1);
  if df2 > 0 && F >= 0
    p = betainc(df2 / (df2 + df1 * F), df2 / 2, df1 / 2);
  else
    p = NaN;
  end
end
%=============================================================================
function p = chiSquareUpperLocal(x, df)
  p = gammaincUpperLocal(x / 2, df / 2);
end
%=============================================================================
function q = gammaincUpperLocal(x, a)
  if x < 0 || a <= 0
    q = NaN;
  elseif x == 0
    q = 1;
  elseif x < a + 1
    q = 1 - gammaincSeriesLocal(x, a);
  else
    q = gammaincFractionLocal(x, a);
  end
end
%=============================================================================
function p = gammaincSeriesLocal(x, a)
  ap = a;
  del = 1 / a;
  sumValue = del;
  for k = 1:200
    ap = ap + 1;
    del = del * x / ap;
    sumValue = sumValue + del;
    if abs(del) < abs(sumValue) * eps
      break
    end
  end
  p = sumValue * exp(-x + a * log(x) - gammaln(a));
end
%=============================================================================
function q = gammaincFractionLocal(x, a)
  fpmin = realmin / eps;
  b = x + 1 - a;
  c = 1 / fpmin;
  d = 1 / max(b, fpmin);
  h = d;
  for k = 1:200
    an = -k * (k - a);
    b = b + 2;
    d = an * d + b;
    if abs(d) < fpmin
      d = fpmin;
    end
    c = b + an / c;
    if abs(c) < fpmin
      c = fpmin;
    end
    d = 1 / d;
    del = d * c;
    h = h * del;
    if abs(del - 1) < eps
      break
    end
  end
  q = exp(-x + a * log(x) - gammaln(a)) * h;
end
%=============================================================================
