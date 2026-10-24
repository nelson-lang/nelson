%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function C = times(A, B)
  if ~isa(B, 'categorical')
    B = categorical(B);
  end
  [aCodes, bCodes] = categorical.expandPairCodes(A.codes, B.codes);
  names = cell(length(A.categoryNames) * length(B.categoryNames), 1);
  p = 1;
  for i = 1:length(A.categoryNames)
    for j = 1:length(B.categoryNames)
      names{p} = [A.categoryNames{i}, ' ', B.categoryNames{j}];
      p = p + 1;
    end
  end
  codes = zeros(size(aCodes));
  valid = aCodes > 0 & bCodes > 0;
  codes(valid) = (aCodes(valid) - 1) * length(B.categoryNames) + bCodes(valid);
  C = categorical.fromCodes(codes, names, false, false);
end
%=============================================================================
