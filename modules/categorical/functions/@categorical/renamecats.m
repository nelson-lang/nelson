%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function A = renamecats(A, oldNames, newNames)
  if nargin == 2
    newNames = oldNames;
    if length(newNames) ~= length(A.categoryNames)
      error('Nelson:renamecats:categoryCountMismatch', 'New category names must match existing categories.');
    end
    A.categoryNames = categorical.checkCategoryNames(newNames, false);
    return
  end
  oldNames = categorical.checkCategoryNames(oldNames, true);
  newNames = categorical.checkCategoryNames(newNames, false);
  if length(oldNames) ~= length(newNames)
    error('Nelson:renamecats:lengthMismatch', 'Old and new category names must have the same length.');
  end
  for k = 1:length(oldNames)
    idx = find(strcmp(A.categoryNames, oldNames{k}), 1);
    if isempty(idx)
      error('Nelson:categorical:categoryNotFound', 'Category not found.');
    end
    A.categoryNames{idx} = newNames{k};
  end
  categorical.checkCategoryNames(A.categoryNames, false);
end
%=============================================================================
