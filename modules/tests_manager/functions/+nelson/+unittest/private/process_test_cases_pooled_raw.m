%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_cases_updated = process_test_cases_pooled_raw(test_cases, nbWorkers, runtimeOptions)
  chunks = worker_pool_chunks(test_cases, nbWorkers);
  workers = worker_pool_create_workers(chunks, runtimeOptions);
  %#ok<NLS0004>
  cleanupWorkers = onCleanup(@() worker_pool_remove_workers(workers));
  nativeResults = run_worker_pool_processes(workers, nbWorkers);
  test_cases_updated = worker_pool_collect_results(chunks, workers, nativeResults);
  test_cases_updated = worker_pool_mark_worker_usage(...
    test_cases_updated, workers, nbWorkers);
  test_cases_updated = worker_pool_retry_missing(test_cases_updated, nbWorkers);
end
%=============================================================================
