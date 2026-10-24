%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function A = removecats(A, oldCategories)
  narginchk(1, 2);
  if nargin == 1
    used = unique(A.codes(:));
    removeCodes = setdiff(1:length(A.categoryNames), used(:)');
  else
    removeCodes = categorical.categoryCodes(oldCategories, A.categoryNames);
  end
  keep = true(length(A.categoryNames), 1);
  keep(removeCodes) = false;
  map = zeros(length(A.categoryNames), 1);
  map(keep) = 1:sum(keep);
  % Remap every code in one vectorized gather: reading and writing A.codes(k)
  % per element copies the whole codes array each iteration, which is O(n^2).
  codes = A.codes;
  nz = codes > 0;
  codes(nz) = map(codes(nz));
  A.codes = codes;
  A.categoryNames = A.categoryNames(keep);
end
%=============================================================================
