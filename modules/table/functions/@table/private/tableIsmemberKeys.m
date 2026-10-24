%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [tf, loc] = tableIsmemberKeys(keysA, keysB)
  tf = false(length(keysA), 1);
  loc = zeros(length(keysA), 1);
  for k = 1:length(keysA)
    found = find(strcmp(keysB, keysA{k}), 1);
    if ~isempty(found)
      tf(k) = true;
      loc(k) = found;
    end
  end
end
%=============================================================================
