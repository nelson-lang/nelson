%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case = worker_pool_apply_payload(test_case, payload, nativeResult)
  outputfile = test_case.outputfile;
  test_case.runfile_result = payload;
  test_case.res_cmd = payload.r;
  test_case.exit_code = payload.r;
  test_case.timed_out = nativeResult.timed_out;
  test_case.stderr = nativeResult.stderr;
  test_case = applyNativeProcessDiagnostics(test_case, nativeResult);
  test_case.msg = '';
  test_case = post_run_test_case(test_case, false);
  if ~isempty(outputfile) && isfile(outputfile)
    try
      rmfile(outputfile);
    end
  end
end
%=============================================================================
