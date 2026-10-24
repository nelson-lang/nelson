%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case_res = post_run_test_case_skip_or_pass(test_case_res)
  if isstruct(test_case_res.runfile_result)
    test_case_res = applyRunfileResultStatus(test_case_res, test_case_res.runfile_result);
    return
  end
  if ~isfile(test_case_res.outputfile)
    msg = '';
    if isfile(test_case_res.redirect_err)
      msg = test_redirect_error_message(test_case_res.redirect_err, test_case_res.res_cmd);
    end
    test_case_res = post_run_fail_result_file(test_case_res, msg);
    return
  end
  [s, ok] = read_runfile_output_file(test_case_res.outputfile); %#ok<NLS0004>
  try
    rmfile(test_case_res.outputfile);
  end
  if ~ok
    test_case_res = post_run_fail_result_file(test_case_res, s.msg);
    return
  end
  test_case_res = applyRunfileResultStatus(test_case_res, s);
  test_case_res = post_run_capture_redirect(test_case_res);
end
