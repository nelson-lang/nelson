%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case = worker_pool_missing_result(test_case, nativeResult)
  test_case.pool_missing_result = true;
  test_case.worker_pool_failure = worker_pool_failure_from_native(nativeResult);
  test_case.status = 'Fail';
  test_case.skip = false;
  test_case.res_cmd = nativeResult.exit_code;
  test_case.exit_code = nativeResult.exit_code;
  test_case.timed_out = nativeResult.timed_out;
  test_case.stdout = nativeResult.stdout;
  test_case.stderr = nativeResult.stderr;
  if nativeResult.timed_out
    test_case.msg = 'ABORTED';
  else
    test_case.msg = 'Worker exited before returning this test result.';
  end
end
%=============================================================================
