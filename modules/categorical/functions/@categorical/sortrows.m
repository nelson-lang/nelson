%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [B, I] = sortrows(A, varargin)
  sortable = A.codes;
  sortable(sortable == 0) = Inf;
  I = (1:size(sortable, 1))';
  for col = size(sortable, 2):-1:1
    [~, order] = sort(sortable(I, col), varargin{:});
    I = I(order);
  end
  B = A(I, :);
end
%=============================================================================
