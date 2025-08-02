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
    dim = d(1);
  end
  if (~isscalar(dim) || dim <= 0)
    error(_('Dimension argument must be a positive integer scalar.'));
  end
  dim = floor(dim);
  idx(1:max(ndims(A), dim)) = {':'};
  idx{dim} = size(A, dim):-1:1;
  B = A(idx{:});
end
%=============================================================================
