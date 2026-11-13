%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function msg = missing_runfile_result_message(test_case_res)
  % The test process returned no result: report what it printed before exiting.
  if ~isempty(test_case_res.redirect_err) && isfile(test_case_res.redirect_err)
    msg = test_redirect_error_message(test_case_res.redirect_err, test_case_res.res_cmd);
    return
  end
  msg = strtrim(test_case_res.stderr);
  if isempty(msg)
    msg = strtrim(test_case_res.msg);
  end
  if isempty(msg)
    % e.g. the test called exit(N), or the process crashed
    msg = ['Test process ended with exit code ', num2str(test_case_res.res_cmd), ...
      ' before returning a result.'];
  end
end
%=============================================================================
