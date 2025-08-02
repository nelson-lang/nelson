%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function B = flipdim(A, dim)
  if (~isscalar(dim) || dim <= 0)
    error(_('Dimension argument must be a positive integer scalar.'));
  end
  dimsize = size(A, floor(dim));
  if (dimsize > 1)
    D(1:ndims(A)) = {':'};
    D{dim} = dimsize:-1:1;
    B = A(D{:});
  else
    B = A;
  end
end
%=============================================================================
