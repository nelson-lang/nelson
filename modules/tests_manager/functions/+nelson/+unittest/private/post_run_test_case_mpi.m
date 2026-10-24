%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case_res = post_run_test_case_mpi(test_case_res) %#ok<NLS0026>
  if isstruct(test_case_res.runfile_result)
    test_case_res = applyRunfileResultStatus(test_case_res, test_case_res.runfile_result);
  elseif isfile(test_case_res.outputfile)
    [s, ok] = read_runfile_output_file(test_case_res.outputfile); %#ok<NLS0004>
    try
      rmfile(test_case_res.outputfile);
    end
    if ~ok
      test_case_res = post_run_fail_result_file(test_case_res, s.msg);
      return
    end
    test_case_res = applyRunfileOutput(test_case_res, s);
    test_case_res.time = s.timing;
    switch (s.r) %#ok<NLS0008>
      case 0
        test_case_res.status = 'Pass';
        test_case_res.skip = false;
      case 1
        test_case_res.status = 'Fail';
        test_case_res.msg = s.msg;
        test_case_res.skip = false;
      case 2
        test_case_res.status = 'Skip';
        test_case_res.skip = true;
        test_case_res.msg = s.msg;
      otherwise
        error('Nelson:tests_manager:unhandledCase', 'Case not managed.');
    end
  else
    test_case_res.status = 'Fail';
  end
  test_case_res = post_run_capture_redirect(test_case_res);
end
