%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_cases = worker_pool_collect_worker(chunk, nativeResult)
  payloads = extractRunfilePayloads(nativeResult.stdout);
  test_cases = chunk;
  for k = 1:length(chunk)
    if k <= length(payloads) && isstruct(payloads(k))
      test_cases(k) = worker_pool_apply_payload(chunk(k), payloads(k), nativeResult);
    else
      test_cases(k) = worker_pool_missing_result(chunk(k), nativeResult);
    end
  end
end
%=============================================================================
