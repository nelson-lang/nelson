%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case_res = post_run_test_case_aborted(test_case_res)
  test_case_res.status = 'Fail';
  test_case_res.msg = ['Test execution was terminated after timeout.'];
  test_case_res.timed_out = true;
  test_case_res.stderr = test_case_res.msg;
end
