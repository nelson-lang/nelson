%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tf = worker_pool_should_inline(runtimeOptions, code, cmd)
  tf = runtimeOptions.WorkerPoolInline && worker_pool_can_inline(code, cmd);
end
%=============================================================================
