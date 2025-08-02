%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function B = squeeze(A)
  if ismatrix(A) 
    B = A;
  else
    sz = size(A);
    sz(sz == 1) = [];
    if ~isscalar(sz)
      B = reshape(A,sz);
    else
      B = reshape(A, sz, 1);
    end
  end
end    
%=============================================================================
