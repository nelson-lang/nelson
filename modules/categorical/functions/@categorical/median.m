%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function M = median(A, varargin)
  if ~A.isOrdinal
    error('Nelson:categorical:requiresOrdinal', '%s requires ordinal categorical arrays.', 'Median');
  end
  S = sort(A(:));
  M = S(ceil(numel(S) / 2));
end
%=============================================================================
