%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function accumulator = run_test_batch_from_native(accumulator, nativeResults, abortedCode)
  for k = 1:length(accumulator)
    [status, message, accumulator(k)] = test_case_native_status(...
      accumulator(k), nativeResults(k));
    aborted = is_aborted_test(status, message, accumulator(k), abortedCode);
    accumulator(k).msg = test_case_message(accumulator(k), message);
    accumulator(k).res_cmd = status;
    accumulator(k).exit_code = status;
    accumulator(k) = post_run_test_case(accumulator(k), aborted);
  end
end
%=============================================================================
