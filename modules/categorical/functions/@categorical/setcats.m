%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function A = setcats(A, newCategories)
  narginchk(2, 2);
  desired = categorical.checkCategoryNames(newCategories, false);
  [tf, map] = categorical.namesMember(A.categoryNames, desired);
  % Remap codes in one vectorized pass (codes outside the new set become
  % undefined): per-element access to A.codes copies the whole array each
  % iteration, which is O(numel^2).
  codes = A.codes;
  nz = codes > 0;
  nzCodes = codes(nz);
  mapped = map(nzCodes);
  mapped(~tf(nzCodes)) = 0;
  codes(nz) = mapped;
  A.codes = codes;
  A.categoryNames = desired(:);
end
%=============================================================================
