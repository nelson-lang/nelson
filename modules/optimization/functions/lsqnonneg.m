%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, resnorm, residual, exitflag, output, lambda] = lsqnonneg(C, d, options)
  if nargin < 2
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if nargin > 3
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
  if nargin < 3
    options = [];
  end
  opts = optimMergeOptions('lsqnonneg', options);
  C = double(C); d = double(d(:));
  [~, n] = size(C);
  x = zeros(n, 1);
  passive = false(n, 1);
  w = C' * (d - C * x);
  iter = 0;
  maxIter = opts.MaxIter;
  if isempty(maxIter)
    maxIter = 3 * n;
  end
  tol = max(size(C)) * norm(C, 1) * eps;
  exitflag = 1;
  while any(~passive & (w > tol)) && iter < maxIter
    [~, t] = max(w .* (~passive));
    passive(t) = true;
    z = zeros(n, 1);
    z(passive) = solveLeastSquares(C(:, passive), d);
    while any(z(passive) <= tol)
      q = passive & (z <= tol);
      alpha = min(x(q) ./ (x(q) - z(q)));
      x = x + alpha * (z - x);
      passive(abs(x) <= tol & passive) = false;
      z = zeros(n, 1);
      if any(passive)
        z(passive) = solveLeastSquares(C(:, passive), d);
      end
      iter = iter + 1;
      if iter >= maxIter
        break
      end
    end
    x = z;
    w = C' * (d - C * x);
    iter = iter + 1;
  end
  if iter >= maxIter
    exitflag = 0;
  end
  residual = d - C * x;
  resnorm = residual' * residual;
  lambda = -w;
  output.iterations = iter;
  output.algorithm = 'active-set nonnegative least squares';
  output.message = exitMessage(exitflag);
end
%=============================================================================
function x = solveLeastSquares(A, b)
  if isempty(A)
    x = [];
  elseif rcond(A' * A) > eps
    x = A \ b;
  else
    x = pinv(A) * b;
  end
end
%=============================================================================
function msg = exitMessage(exitflag)
  if exitflag > 0
    msg = _('Optimization converged.');
  else
    msg = _('Maximum number of iterations reached.');
  end
end
%=============================================================================
