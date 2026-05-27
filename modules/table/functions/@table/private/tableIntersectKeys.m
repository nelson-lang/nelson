%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [ia, ib] = tableIntersectKeys(keysA, keysB)
  ia = [];
  ib = [];
  used = {};
  for k = 1:length(keysA)
    found = find(strcmp(keysB, keysA{k}), 1);
    if ~isempty(found) && ~any(strcmp(used, keysA{k}))
      ia(end + 1) = k;
      ib(end + 1) = found;
      used{end + 1} = keysA{k};
    end
  end
  ia = ia(:);
  ib = ib(:);
end
%=============================================================================
