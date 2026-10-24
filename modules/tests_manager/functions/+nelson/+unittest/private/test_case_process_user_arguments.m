%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function args = test_case_process_user_arguments(test_case)
  args = '';
  args = append_process_option(args, should_disable_audio(test_case), '--no-audio');
  args = append_process_option(args, ~test_case.options.python_environment_required, '--without-python');
  args = append_process_option(args, ~test_case.options.julia_environment_required, '--without-julia');
end
%=============================================================================
