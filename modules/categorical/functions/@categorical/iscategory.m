%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = iscategory(A, names)
  narginchk(2, 2);
  if isa(names, 'pattern')
    tf = ~isempty(categorical.categoryCodes(names, A.categoryNames));
    return
  end
  if isstring(names)
    names = cellstr(names);
  elseif ischar(names)
    names = {names};
  end
  tf = false(size(names));
  for k = 1:numel(names)
    tf(k) = any(strcmp(A.categoryNames, names{k}));
  end
end
%=============================================================================
