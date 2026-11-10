%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, flag, relres, iter, resvec] = tfqmr(A, b, tol, maxit, M1, M2, x0)
  % transpose-free quasi-minimal residual solver for A*x = b, right preconditioned by M = M1*M2
  narginchk(2, 7);
  nargoutchk(0, 5);
  n = size(b, 1);
  if ~iscolumn(b)
    error('Nelson:linear_algebra:expectedVector', 'Vector expected.');
  end
  if isa(A, 'function_handle')
    applyA = A;
  else
    if size(A, 1) ~= size(A, 2)
      error('Nelson:linear_algebra:expectedSquareMatrix', 'Square matrix expected.');
    end
    if size(A, 1) ~= n
      error('Nelson:linear_algebra:dimensionMismatch', 'Matrix and vector dimensions must agree.');
    end
    applyA = @(v) A * v;
  end
  if nargin < 3 || isempty(tol)
    tol = 1e-6;
  elseif ~(isnumeric(tol) && isscalar(tol) && tol >= 0)
    error('Nelson:linear_algebra:invalidTolerance', 'Tolerance must be a nonnegative scalar.');
  end
  if nargin < 4 || isempty(maxit)
    maxit = min(n, 20);
  end
  preconditioners = {};
  if nargin > 4 && ~isempty(M1)
    preconditioners{end + 1} = M1;
  end
  if nargin > 5 && ~isempty(M2)
    preconditioners{end + 1} = M2;
  end
  proto = zeros(1, 'like', b);
  if ~isa(A, 'function_handle')
    proto = proto + zeros(1, 'like', full(A(1)));
  end
  if nargin > 6 && ~isempty(x0)
    if numel(x0) ~= n
      error('Nelson:linear_algebra:initialGuessDimensionMismatch', 'Initial guess dimensions must agree.');
    end
    x = x0(:) + zeros(n, 1, 'like', proto);
  else
    x = zeros(n, 1, 'like', proto);
  end
  normb = norm(b);
  r = b - applyA(x);
  resvec = zeros(2 * maxit + 1, 1);
  resvec(1) = norm(r);
  if normb == 0
    normb = 1;
    if resvec(1) == 0
      x = zeros(n, 1, 'like', proto);
    end
  end
  flag = 1;
  iter = 0;
  relres = resvec(1) / normb;
  if relres <= tol
    flag = 0;
    resvec = resvec(1);
    return
  end
  xBest = x;
  best = resvec(1);
  bestIter = 0;
  w = r;
  u = r;
  uHat = applyPreconditioners(preconditioners, u);
  Au = applyA(uHat);
  v = Au;
  d = zeros(n, 1, 'like', x);
  tau = resvec(1);
  theta = 0;
  eta = 0;
  rTilde = r;
  rho = rTilde' * r;
  alpha = 0;
  last = 1;
  for half = 1:2 * maxit
    even = mod(half, 2) == 1;
    if even
      sigma = rTilde' * v;
      if sigma == 0
        flag = 4;
        break
      end
      alpha = rho / sigma;
      uNext = u - alpha * v;
    end
    w = w - alpha * Au;
    d = uHat + (theta ^ 2 / alpha) * eta * d;
    theta = norm(w) / tau;
    c = 1 / sqrt(1 + theta ^ 2);
    tau = tau * theta * c;
    eta = c ^ 2 * alpha;
    x = x + eta * d;
    last = half + 1;
    current = norm(b - applyA(x));
    resvec(half + 1) = current;
    if current < best
      best = current;
      xBest = x;
      bestIter = floor(half / 2);
    end
    if current <= tol * normb
      flag = 0;
      iter = floor(half / 2);
      relres = current / normb;
      resvec = resvec(1:half + 1);
      return
    end
    if even
      u = uNext;
      uHat = applyPreconditioners(preconditioners, u);
      Au = applyA(uHat);
    else
      rhoNext = rTilde' * w;
      if rho == 0
        flag = 4;
        break
      end
      beta = rhoNext / rho;
      uNew = w + beta * u;
      uHat = applyPreconditioners(preconditioners, uNew);
      AuNew = applyA(uHat);
      v = AuNew + beta * (Au + beta * v);
      u = uNew;
      Au = AuNew;
      rho = rhoNext;
    end
  end
  x = xBest;
  relres = best / normb;
  iter = bestIter;
  resvec = resvec(1:last);
end
%=============================================================================
function y = applyPreconditioners(preconditioners, y)
  % applies M2 \ (M1 \ y), each preconditioner being a matrix or a function handle
  for k = 1:numel(preconditioners)
    M = preconditioners{k};
    if isa(M, 'function_handle')
      y = M(y);
    else
      y = M \ y;
    end
  end
end
%=============================================================================
