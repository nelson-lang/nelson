%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [M, I] = max(A, varargin)
  if ~A.isOrdinal
    error('Nelson:categorical:requiresOrdinal', '%s requires ordinal categorical arrays.', 'Maximum');
  end
  sortable = A.codes;
  sortable(sortable == 0) = -Inf;
  flat = sortable(:);
  maxValue = -Inf;
  I = 0;
  for k = 1:length(flat)
    if flat(k) > maxValue
      maxValue = flat(k);
      I = k;
    end
  end
  M = A(I);
end
%=============================================================================
