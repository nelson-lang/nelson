%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_case = assign_test_case_launcher(test_case, cmd)
  test_launcher_backend(); % validate NELSON_UNITTEST_LAUNCHER
  mpi = test_case.options.mpi_mode;
  if test_case.options.gui_mode
    test_case.command = build_command_nelson_gui(cmd, mpi);
    [test_case.executable, test_case.process_arguments] = build_native_nelson_gui(cmd, mpi);
  elseif test_case.options.adv_cli_mode
    test_case.command = build_command_nelson_adv_cli(cmd, mpi);
    [test_case.executable, test_case.process_arguments] = build_native_nelson_adv_cli(cmd, mpi);
  else
    test_case.command = build_command_nelson_cli(cmd, mpi);
    [test_case.executable, test_case.process_arguments] = build_native_nelson_cli(cmd, mpi);
  end
end
%=============================================================================
