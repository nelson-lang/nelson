%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function X = pageinv(A)
  % inverse of every page with the device inv; other inputs run on the host
  sz = size(A);
  if ~(isfloat(A) && isreal(A) && sz(1) == sz(2) && sz(1) > 0)
    X = __gpuArray_host__('pageinv', A);
    return
  end
  n = sz(1);
  pages = prod(sz(3:end));
  P = reshape(A, n, n, pages);
  X = zeros(n, n, pages, 'like', P);
  for k = 1:pages
    X(:, :, k) = inv(P(:, :, k));
  end
  X = reshape(X, sz);
end
%=============================================================================
