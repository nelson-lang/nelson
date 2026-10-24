%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Upper Hessenberg reduction using Householder reflectors.
% H = hess(A)        returns the upper Hessenberg form H.
% [P, H] = hess(A)   returns unitary P and H such that A = P * H * P'.
%=============================================================================
function varargout = hess(varargin)
  narginchk(1, 1);
  nargoutchk(0, 2);

  A = varargin{1};
  if ~isnumeric(A) || ndims(A) > 2 || size(A, 1) ~= size(A, 2)
    error('Nelson:linear_algebra:squareMatrixExpected', '%s expected.', 'Square matrix');
  end

  n = size(A, 1);
  H = A;
  P = eye(n, class(A));

  for k = 1:n - 2
    x = H(k + 1:n, k);
    if norm(x(2:end)) == 0
      continue;
    end
    if x(1) == 0
      s = 1;
    else
      s = x(1) / abs(x(1));
    end
    alpha = -s * norm(x);
    v = x;
    v(1) = v(1) - alpha;
    nv = norm(v);
    if nv == 0
      continue;
    end
    v = v / nv;
    % Apply H_k = I - 2*v*v' on the left and on the right.
    H(k + 1:n, :) = H(k + 1:n, :) - 2 * v * (v' * H(k + 1:n, :));
    H(:, k + 1:n) = H(:, k + 1:n) - 2 * (H(:, k + 1:n) * v) * v';
    % Accumulate P = Q_1 * Q_2 * ... so that A = P * H * P'.
    P(:, k + 1:n) = P(:, k + 1:n) - 2 * (P(:, k + 1:n) * v) * v';
  end

  if nargout <= 1
    varargout{1} = H;
  else
    varargout{1} = P;
    varargout{2} = H;
  end
end
%=============================================================================
