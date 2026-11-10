%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Y = pagepinv(X, tol)
  % pseudoinverse of every page of X
  narginchk(1, 2);
  if ~isfloat(X)
    error('Nelson:linalg:pageFloatInput', 'Inputs must be single or double arrays.');
  end
  sz = size(X);
  pageSize = sz(3:end);
  count = prod(pageSize);
  Xr = reshape(X, sz(1), sz(2), count);
  Y = zeros([sz(2), sz(1), count], 'like', Xr);
  for k = 1:count
    if nargin < 2
      Y(:, :, k) = pinv(Xr(:, :, k));
    else
      Y(:, :, k) = pinv(Xr(:, :, k), tol);
    end
  end
  Y = reshape(Y, [sz(2), sz(1), pageSize]);
end
%=============================================================================
