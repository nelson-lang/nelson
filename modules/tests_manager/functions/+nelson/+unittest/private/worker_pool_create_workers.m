%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function workers = worker_pool_create_workers(chunks, runtimeOptions)
  workers = repmat(worker_pool_empty_worker(), 1, length(chunks));
  privateDir = fileparts(nfilename('fullpathext'), 'path');
  worker_pool_cleanup_stale_scripts();
  for k = 1:length(chunks)
    workers(k) = worker_pool_create_worker(chunks{k}, privateDir, runtimeOptions);
  end
end
%=============================================================================
