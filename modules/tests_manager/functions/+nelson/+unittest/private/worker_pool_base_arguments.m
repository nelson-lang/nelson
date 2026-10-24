%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function args = worker_pool_base_arguments(cmd, timeout)
  args = [cmd, ' --quiet --no-user-startup --timeout ', int2str(timeout)];
end
%=============================================================================
