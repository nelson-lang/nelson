%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case_res = post_run_test_case_skip_or_fail(test_case_res) %#ok<NLS0026>
  if isstruct(test_case_res.runfile_result)
    test_case_res = applyRunfileResultStatus(test_case_res, test_case_res.runfile_result);
  elseif isfile(test_case_res.outputfile)
    [s, ok] = read_runfile_output_file(test_case_res.outputfile); %#ok<NLS0004>
    try
      rmfile(test_case_res.outputfile);
    end
    if ok
      test_case_res = applyRunfileResultStatus(test_case_res, s);
    else
      test_case_res = post_run_fail_result_file(test_case_res, s.msg);
    end
  elseif isfile(test_case_res.redirect_err)
    test_case_res.status = 'Fail';
    test_case_res.skip = false;
    test_case_res = captureRedirectError(test_case_res);
    test_case_res.msg = test_redirect_error_message(test_case_res.redirect_err, test_case_res.res_cmd);
  else
    test_case_res.status = 'Fail';
    test_case_res.msg = ['Error code not caught:', ' ', num2str(test_case_res.res_cmd)];
  end
  test_case_res = post_run_capture_redirect(test_case_res);
end
