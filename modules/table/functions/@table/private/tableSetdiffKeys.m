%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ia = tableSetdiffKeys(keysA, keysB)
  ia = [];
  used = {};
  for k = 1:length(keysA)
    if isempty(find(strcmp(keysB, keysA{k}), 1)) && ~any(strcmp(used, keysA{k}))
      ia(end + 1) = k;
      used{end + 1} = keysA{k};
    end
  end
  ia = ia(:);
end
%=============================================================================
