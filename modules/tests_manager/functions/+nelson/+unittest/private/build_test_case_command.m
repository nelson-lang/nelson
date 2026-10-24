%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [cmd, command_filename, outputfile, redirect_err] = build_test_case_command(test_case, private_dir)
  cmd = test_case_process_options(test_case);
  userArguments = test_case_process_user_arguments(test_case);
  command_filename = '';
  outputfile = '';
  redirect_err = '';
  [cmd, command_filename, outputfile, redirect_err] = ...
    buildFileRunCommand(cmd, test_case, private_dir, userArguments);
end
%=============================================================================
