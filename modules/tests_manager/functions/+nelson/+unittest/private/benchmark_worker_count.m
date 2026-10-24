%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function workers = benchmark_worker_count(availableThreads)
  workers = 1;
  if availableThreads > 5
    workers = 2;
  end
end
%=============================================================================
