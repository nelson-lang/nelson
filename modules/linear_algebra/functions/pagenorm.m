%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Y = pagenorm(X, p)
  narginchk(1, 2);
  nargoutchk(0, 1);
  if ~isnumeric(X)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Input');
  end
  if nargin < 2
    p = 2;
  end
  sz = size(X);
  m = sz(1);
  n = sz(2);
  pagesSize = sz(3:end);
  npages = prod(pagesSize);
  Xr = reshape(X, m, n, npages);
  Y = zeros(1, 1, npages, class(X));
  for k = 1:npages
    Y(1, 1, k) = norm(Xr(:, :, k), p);
  end
  outSize = [1, 1, pagesSize];
  Y = reshape(Y, outSize);
end
%=============================================================================
