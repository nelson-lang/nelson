%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = rank(A, tol)
  S = svd(A);
  if (nargin == 1)
    maxS = max(S);
    if isempty(maxS)
      epsMax = eps;  
    else
      epsMax = eps(maxS);
    end
    tol = max(size(A)) * epsMax;
  end
  R = sum(S > tol);
end
%=============================================================================
