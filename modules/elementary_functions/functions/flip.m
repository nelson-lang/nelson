%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function B = flip(A, dim)
  if (nargin == 1)
    sz = size(A);
    d = 1:length(sz);
    d(sz <= 1) = [];
    if isempty(d)
      dim = 1;
    else
      dim = d(1);
    end
  end
  if (~(isnumeric(dim) || islogical(dim)) || ~isscalar(dim) || ~isreal(dim) || ~isfinite(dim) || dim <= 0 || dim ~= fix(dim))
    error('Nelson:elementary_functions:dimensionArgumentPositiveIntegerScalar', '%s must be a positive integer scalar.', 'Dimension argument');
  end
  dim = double(dim);
  if (~issparse(A) && (isnumeric(A) || islogical(A) || ischar(A)))
    B = __flip__(A, dim);
    return
  end
  idx(1:max(ndims(A), dim)) = {':'};
  idx{dim} = size(A, dim):-1:1;
  B = A(idx{:});
end
%=============================================================================
