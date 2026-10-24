%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--SEQUENTIAL TEST REQUIRED-->
skip_if_no_native_unittest_runner();
%=============================================================================
binpath = modulepath('nelson', 'bin');
nelson_exe = [binpath, '/nelson-cli'];
if ispc()
  nelson_exe = [nelson_exe, '.exe'];
end
case_file = [modulepath('tests_manager', 'tests'), ...
  '/process_supervisor_shutdown_case.m'];
process_arguments = ['--quiet --no-ipc --no-user-modules -e "run(', ...
  nelsonStringLiteral(case_file), ')"'];
jobs = run_process_jobs({nelson_exe}, {process_arguments}, 30, ...
  {'shutdown'}, 1, {'shutdown'}, {'shutdown'});
results = run_processes(jobs, 1);
asserts.isequal(results.exit_code, 0);
asserts.isequal(results.reason, 'ok');
asserts.isfalse(contains(results.stderr, 'terminating'));
%=============================================================================
