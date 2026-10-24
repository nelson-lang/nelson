%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_cases = worker_pool_collect_results(chunks, workers, nativeResults)
  test_cases = [];
  for k = 1:length(chunks)
    test_cases = [test_cases; worker_pool_collect_worker(chunks{k}, nativeResults(k))];
    run_removeFile(workers(k).command_filename);
  end
  test_cases = worker_pool_sort_results(test_cases);
end
%=============================================================================
