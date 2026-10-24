%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function test_case_res = post_run_capture_redirect(test_case_res)
  if ~isempty(test_case_res.redirect_err) && isfile(test_case_res.redirect_err)
    test_case_res = captureRedirectError(test_case_res);
    try
      rmfile(test_case_res.redirect_err);
    end
  end
end
%=============================================================================
