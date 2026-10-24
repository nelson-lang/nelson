%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function worker = worker_pool_empty_worker()
  worker = struct('executable', '', 'process_arguments', '', ...
    'launcher_timeout', 0, 'command_filename', '');
end
%=============================================================================
