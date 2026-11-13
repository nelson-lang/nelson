%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function X = lsqminnorm(A, B, varargin)
  % minimum norm least-squares solution of A*X = B through a complete orthogonal decomposition
  narginchk(2, 4);
  if ~(isfloat(A) && isfloat(B))
    error('Nelson:linalg:pageFloatInput', 'Inputs must be single or double arrays.');
  end
  if size(A, 1) ~= size(B, 1)
    error('Nelson:linalg:pageRowsMismatch', 'The two inputs must have the same number of rows.');
  end
  tol = [];
  warnRank = false;
  for k = 1:numel(varargin)
    option = varargin{k};
    if (ischar(option) || isstring(option)) && any(strcmpi(char(option), {'warn', 'nowarn'}))
      warnRank = strcmpi(char(option), 'warn');
    elseif isnumeric(option) && isreal(option) && (isempty(option) || isscalar(option))
      tol = option;
    else
      error('Nelson:lsqminnorm:invalidOption', 'Options must be a tolerance or ''warn'' / ''nowarn''.');
    end
  end
  if issparse(A)
    A = full(A);
  end
  if issparse(B)
    B = full(B);
  end
  [m, n] = size(A);
  if isempty(tol)
    tol = max(m, n) * eps(norm(A, 'fro'));
  end
  proto = zeros(1, 'like', A) + zeros(1, 'like', B);
  X = zeros([n, size(B, 2)], 'like', proto);
  if m == 0 || n == 0
    return
  end
  [Q, R, perm] = qr(A, 0);
  r = sum(abs(diag(R)) > tol);
  if warnRank && r < min(m, n)
    warning('Nelson:lsqminnorm:rankDeficient', 'Rank deficient, rank = %d, tol = %e.', r, double(tol));
  end
  if r == 0
    return
  end
  C = Q(:, 1:r)' * B;
  if r < n
    [Z, T] = qr(R(1:r, :)', 0);
    X(perm, :) = Z * (T' \ C);
  else
    X(perm, :) = R(1:r, 1:r) \ C;
  end
end
%=============================================================================
