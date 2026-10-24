%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [B, I] = topkrows(A, k, vars, direction)
  narginchk(2, 4);
  if nargin < 3
    vars = [];
  end
  if nargin < 4
    direction = 'descend';
  end
  [S, order] = sortrows(A, vars, direction);
  n = min(k, height(S));
  B = S(1:n, :);
  I = order(1:n);
end
%=============================================================================
