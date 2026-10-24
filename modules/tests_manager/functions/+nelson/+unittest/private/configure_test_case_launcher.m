%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case = configure_test_case_launcher(test_case)
  private_dir = fileparts(nfilename('fullpathext'), 'path');
  [cmd, command_filename, outputfile, redirect_err] = build_test_case_command(test_case, private_dir); %#ok<NLS0004>
  test_case = assign_test_case_launcher(test_case, cmd);
  test_case.outputfile = outputfile;
  test_case.redirect_err = redirect_err;
  test_case.command_filename = command_filename;
end
%=============================================================================
