%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function B = subsref(A, S)
  if strcmp(S(1).type, '()')
    B = categorical.fromCodes(A.codes(S(1).subs{:}), A.categoryNames, A.isOrdinal, A.isProtected);
    if length(S) > 1
      B = subsref(B, S(2:end));
    end
    return
  end
  if strcmp(S(1).type, '{}')
    error('Nelson:categorical:braceIndexingNotSupported', 'Brace indexing is not supported for categorical arrays.');
  end
  if strcmp(S(1).type, '.')
    switch S(1).subs
      case 'codes'
        B = A.codes;
      case 'categoryNames'
        B = A.categoryNames;
      case 'isProtected'
        B = A.isProtected;
      case 'isOrdinal'
        B = A.isOrdinal;
      otherwise
        error('Nelson:categorical:dotIndexingNotSupported', 'Dot indexing is not supported for categorical arrays.');
    end
    if length(S) > 1
      B = subsref(B, S(2:end));
    end
    return
  end
  error('Nelson:categorical:invalidIndexing', 'Invalid categorical indexing.');
end
%=============================================================================
