%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function test_case_res = post_run_fail_result_file(test_case_res, msg)
  test_case_res.status = 'Fail';
  test_case_res.skip = false;
  test_case_res.msg = msg;
end
%=============================================================================
