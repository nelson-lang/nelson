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

jobs = run_process_jobs({nelson_exe; nelson_exe; nelson_exe}, ...
  {'--quiet --no-ipc --no-user-modules -e "disp(''typed_one'')"'; ...
  '--quiet --no-ipc --no-user-modules -e "disp(''typed_two'')"'; ...
  '--quiet --no-ipc --no-user-modules -e "pause(5)"'}, ...
  [30, 30, 1], {'pool'; 'direct'; 'timeout'}, [7, 3, 9], ...
  {'worker-a'; 'test-b'; 'timeout-c'}, {'job-worker-a'; 'job-test-b'; ...
  'job-timeout-c'});
results = run_processes(jobs, 3);

asserts.isequal(length(results), 3);
asserts.istrue(isfield(results, 'kind'));
asserts.istrue(isfield(results, 'index'));
asserts.istrue(isfield(results, 'metadata'));
asserts.istrue(isfield(results, 'job_id'));
asserts.istrue(isfield(results, 'pid'));
asserts.istrue(isfield(results, 'timeout'));
asserts.istrue(isfield(results, 'reason'));
asserts.istrue(isfield(results, 'process_policy'));
asserts.istrue(isfield(results, 'executable'));
asserts.istrue(isfield(results, 'process_arguments'));
asserts.isequal(results(1).exit_code, 0);
asserts.isequal(results(2).exit_code, 0);
asserts.istrue(results(3).exit_code ~= 0);
asserts.isequal(results(1).kind, 'pool');
asserts.isequal(results(2).kind, 'direct');
asserts.isequal(results(3).kind, 'timeout');
asserts.isequal(results(1).index, 7);
asserts.isequal(results(2).index, 3);
asserts.isequal(results(3).index, 9);
asserts.isequal(results(1).metadata, 'worker-a');
asserts.isequal(results(2).metadata, 'test-b');
asserts.isequal(results(3).metadata, 'timeout-c');
asserts.isequal(results(1).job_id, 'job-worker-a');
asserts.isequal(results(2).job_id, 'job-test-b');
asserts.isequal(results(3).job_id, 'job-timeout-c');
asserts.istrue(results(1).pid > 0);
asserts.istrue(results(2).pid > 0);
asserts.istrue(results(3).pid > 0);
asserts.isequal(results(1).timeout, 30);
asserts.isequal(results(2).timeout, 30);
asserts.isequal(results(3).timeout, 1);
asserts.isequal(results(1).reason, 'ok');
asserts.isequal(results(2).reason, 'ok');
asserts.isequal(results(3).reason, 'timeout');
asserts.istrue(contains(results(1).process_policy, 'windows') || ...
  contains(results(1).process_policy, 'posix'));
asserts.istrue(results(3).timed_out);
asserts.isequal(results(1).executable, nelson_exe);
asserts.isequal(results(3).executable, nelson_exe);
asserts.istrue(contains(results(1).process_arguments, 'typed_one'));
asserts.istrue(contains(results(3).process_arguments, 'pause(5)'));
asserts.istrue(contains(results(1).stdout, 'typed_one'));
asserts.istrue(contains(results(2).stdout, 'typed_two'));

workerJobs = repmat(struct('executable', '', 'process_arguments', '', ...
  'launcher_timeout', 30, 'command_filename', ''), 1, 2);
workerJobs(1).executable = nelson_exe;
workerJobs(1).process_arguments = '--quiet --no-ipc --no-user-modules -e "disp(''record_one'')"';
workerJobs(1).command_filename = 'record-worker-a';
workerJobs(2).executable = nelson_exe;
workerJobs(2).process_arguments = '--quiet --no-ipc --no-user-modules -e "disp(''record_two'')"';
workerJobs(2).command_filename = 'record-worker-b';
recordResults = run_processes(workerJobs, 2);

asserts.isequal(length(recordResults), 2);
asserts.isequal(recordResults(1).kind, 'pool');
asserts.isequal(recordResults(2).kind, 'pool');
asserts.isequal(recordResults(1).index, 1);
asserts.isequal(recordResults(2).index, 2);
asserts.isequal(recordResults(1).metadata, 'record-worker-a');
asserts.isequal(recordResults(2).metadata, 'record-worker-b');
asserts.isequal(recordResults(1).job_id, 'pool_1');
asserts.isequal(recordResults(2).job_id, 'pool_2');
asserts.istrue(contains(recordResults(1).stdout, 'record_one'));
asserts.istrue(contains(recordResults(2).stdout, 'record_two'));

legacyAccepted = true;
try
  run_processes({nelson_exe}, {'--quiet --no-ipc --no-user-modules -e "disp(1)"'}, uint64(30));
catch
  legacyAccepted = false;
end
asserts.isfalse(legacyAccepted);
cleaned = cleanup_processes(false);
asserts.isequal(cleaned, 0);

slowJobs = run_process_jobs({nelson_exe}, ...
  {'--quiet --no-ipc --no-user-modules -e "pause(20)"'}, ...
  30, {'direct'}, 1, {'cleanup'}, {'cleanup'});
slowHandle = __nelson_unittest_process_supervisor_start__(slowJobs, 1);
pause(0.1);
cleaned = cleanup_processes(true);
asserts.isequal(cleaned, 1);
asserts.istrue(isempty(__nelson_unittest_process_supervisor_poll__(slowHandle, 0)));
%=============================================================================
