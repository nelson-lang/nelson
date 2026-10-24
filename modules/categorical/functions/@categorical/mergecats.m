%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function A = mergecats(A, oldCategories, newCategory)
  narginchk(2, 3);
  if nargin < 3
    oldCodes = categorical.categoryCodes(oldCategories, A.categoryNames);
    if isempty(oldCodes)
      return
    end
    newCategory = A.categoryNames{oldCodes(1)};
  else
    oldCodes = categorical.categoryCodes(oldCategories, A.categoryNames);
  end
  newCategory = categorical.checkCategoryNames(newCategory, false);
  newCategory = newCategory{1};
  if isempty(oldCodes)
    return
  end
  target = find(strcmp(A.categoryNames, newCategory), 1);
  if isempty(target)
    A.categoryNames{oldCodes(1)} = newCategory;
    target = oldCodes(1);
  end
  % Redirect every merged code in one pass: reading and writing A.codes(k) per
  % element copies the whole codes array each iteration, which is O(n^2).
  codes = A.codes;
  codes(ismember(codes, oldCodes)) = target;
  A.codes = codes;
  removeCodes = setdiff(oldCodes, target);
  if ~isempty(removeCodes)
    A = removecats(A, A.categoryNames(removeCodes));
  end
end
%=============================================================================
