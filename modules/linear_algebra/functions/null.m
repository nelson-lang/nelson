%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Z = null(A, opt)
  narginchk(1, 2);
  nargoutchk(0, 1);
  if ~isnumeric(A) || ndims(A) > 2
    error('Nelson:null:invalidInput', 'Input must be a 2D numeric matrix.');
  end
  [m, n] = size(A);
  if nargin == 2
    if ~(ischar(opt) || (isstring(opt) && isscalar(opt))) || ~strcmpi(char(opt), 'r')
      error('Nelson:null:invalidOption', 'Second argument must be ''r''.');
    end
    % rational basis via reduced row echelon form
    [R, pivcol] = rref(A);
    r = numel(pivcol);
    nopiv = 1:n;
    nopiv(pivcol) = [];
    Z = zeros(n, n - r);
    if n > r
      Z(nopiv, :) = eye(n - r);
      Z(pivcol, :) = -R(1:r, nopiv);
    end
    return;
  end
  % orthonormal basis via singular value decomposition
  if isempty(A)
    Z = zeros(n, n);
    return;
  end
  [~, S, V] = svd(A);
  if m > 1
    s = diag(S);
  else
    s = S(1, 1);
  end
  tol = max(m, n) * eps(max(s));
  r = sum(s > tol);
  Z = V(:, r + 1:n);
end
%=============================================================================
