%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function A = reordercats(A, newOrder)
  narginchk(1, 2);
  if nargin == 1
    newOrder = sort(A.categoryNames);
  elseif isnumeric(newOrder)
    newOrder = A.categoryNames(newOrder);
  else
    newOrder = categorical.checkCategoryNames(newOrder, false);
  end
  [tf, map] = categorical.namesMember(A.categoryNames, newOrder);
  if length(newOrder) ~= length(A.categoryNames) || ~all(tf)
    error('Nelson:reordercats:invalidOrder', 'New category order must contain all categories.');
  end
  % Remap codes in one vectorized gather: per-element access to A.codes copies
  % the whole array each iteration, which is O(numel^2).
  codes = A.codes;
  nz = codes > 0;
  codes(nz) = map(codes(nz));
  A.codes = codes;
  A.categoryNames = newOrder(:);
end
%=============================================================================
