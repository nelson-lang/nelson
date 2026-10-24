%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function memory = optimFminuncLBFGSUpdate(memory, s, y, maxMemory)
  entry.s = s;
  entry.y = y;
  entry.rho = 1 / max(y' * s, eps);
  memory{end + 1} = entry;
  while numel(memory) > maxMemory
    memory(1) = [];
  end
end
%=============================================================================
