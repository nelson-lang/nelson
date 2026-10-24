%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case_cleaned = post_run_test_case(test_case_res, aborted) %#ok<NLS0026>
  if ~isempty(test_case_res.command_filename) && isfile(test_case_res.command_filename)
    try
      rmfile(test_case_res.command_filename);
    end
  end
  if test_case_res.options.mpi_mode
    test_case_cleaned = post_run_test_case_mpi(test_case_res);
  else
    if (aborted)
      test_case_cleaned = post_run_test_case_aborted(test_case_res);
    elseif test_case_res.skip
      test_case_cleaned = deep_copy_test_case(test_case_res);
    elseif test_case_res.res_cmd ~= 1
      test_case_cleaned = post_run_test_case_skip_or_fail(test_case_res);
    else
      test_case_cleaned = post_run_test_case_skip_or_pass(test_case_res);
    end
  end
end
