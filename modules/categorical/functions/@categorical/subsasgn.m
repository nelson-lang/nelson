%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function A = subsasgn(A, S, value)
  if ~strcmp(S(1).type, '()') || length(S) ~= 1
    error('Nelson:categorical:invalidAssignment', 'Invalid categorical assignment.');
  end
  if isnumeric(value) && isempty(value)
    A.codes(S(1).subs{:}) = [];
    return
  end
  if ischar(value)
    value = {value};
  end
  if ~isa(value, 'categorical')
    value = categorical(value);
  end
  [rhsCodes, names] = categorical.convertCodesForAssignment(A, value);
  A.codes(S(1).subs{:}) = rhsCodes;
  A.categoryNames = names;
end
%=============================================================================
