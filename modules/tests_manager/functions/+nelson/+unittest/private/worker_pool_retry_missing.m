%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_cases = worker_pool_retry_missing(test_cases, nbWorkers)
  missing = worker_pool_missing_indices(test_cases);
  if isempty(missing)
    return
  end
  abortedCode = launcher_aborted_code();
  failures = {test_cases(missing).worker_pool_failure};
  previousForceNativeRunner = getenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER');
  cleanupForceNativeRunner = onCleanup(...
    @() setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', previousForceNativeRunner)); %#ok<NLS0004>
  setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', 'TRUE');
  retried = run_test_batch(test_cases(missing), abortedCode, nbWorkers);
  for k = 1:length(missing)
    retried(k).pool_missing_result = false;
    retried(k).worker_pool_used = true;
    retried(k).worker_pool_fallback = true;
    retried(k).worker_pool_failure = failures{k};
    test_cases(missing(k)) = retried(k);
  end
end
%=============================================================================
