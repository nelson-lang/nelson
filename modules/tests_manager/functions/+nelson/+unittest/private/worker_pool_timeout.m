%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function timeout = worker_pool_timeout(chunk)
  timeout = 0;
  for k = 1:length(chunk)
    timeout = timeout + chunk(k).launcher_timeout;
  end
end
%=============================================================================
