%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [X, rcondB] = pagemrdivide(B, A)
  % page-wise right division B(:,:,k) / A(:,:,k) with expansion of the page dimensions
  narginchk(2, 2);
  nargoutchk(0, 2);
  [X, rcondB] = pageSolve(B, A, false, false);
  if nargout > 1
    [~, rcondB] = pageSolve(pagectranspose(A), pagectranspose(B), true, true);
  end
end
%=============================================================================
