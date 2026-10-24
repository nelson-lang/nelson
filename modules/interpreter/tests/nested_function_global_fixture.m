%=============================================================================
% Copyright (c) 2026 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = nested_function_global_fixture()
  global nestedGlobalCounter
  global nestedGlobalGrid
  nestedGlobalCounter = 0;
  nestedGlobalGrid = zeros(2, 2);
  for k = 1:3
    bump();
  end
  r = [nestedGlobalCounter, nestedGlobalGrid(1, 1)];
  function bump()
    % Both names are declared 'global' in the parent; the nested function must
    % see and update the shared globals (plain read/write and indexed assignment).
    nestedGlobalCounter = nestedGlobalCounter + 1;
    nestedGlobalGrid(nestedGlobalGrid == nestedGlobalGrid) = nestedGlobalCounter;
  end
end
%=============================================================================
