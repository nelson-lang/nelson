%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [B, I] = topkrows(A, k, col, direction)
  narginchk(2, 4);
  if nargin < 4
    direction = 'descend';
  end
  if nargin < 3 || isempty(col)
    col = 1:size(A, 2);
  end
  k = min(k, size(A, 1));
  if nargout > 1
    [S, idx] = sortrows(A, col, direction);
    I = idx(1:k);
  else
    S = sortrows(A, col, direction);
  end
  B = S(1:k, :);
end
%=============================================================================
