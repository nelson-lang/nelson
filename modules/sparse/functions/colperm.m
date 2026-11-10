%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function p = colperm(S)
  % column permutation that orders columns by increasing nonzero count
  narginchk(1, 1);
  if ~ismatrix(S)
    error('Nelson:colperm:invalidInput', 'Input must be a 2-D matrix.');
  end
  [~, p] = sort(full(sum(S ~= 0, 1)));
end
%=============================================================================
