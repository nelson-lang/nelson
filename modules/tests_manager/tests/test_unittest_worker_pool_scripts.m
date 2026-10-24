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
skip_if_no_native_unittest_runner();
%=============================================================================
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
tmpBase = [[tempdir(), ''], 'test_unittest_worker_pool_script_', createGUID()];
files = {[tmpBase, '_one.m']; [tmpBase, '_two.m']};
cleanupCommand = '';
for k = 1:length(files)
  cleanupCommand = [cleanupCommand, removeCommand(files{k}), newline];
end
%#ok<NLS0004>
cleanupFiles = onCleanup(@() evalc(cleanupCommand));
filewrite(files{1}, {'% <--REUSE PROCESS-->'; 'asserts.istrue(true);'});
filewrite(files{2}, {'% <--REUSE PROCESS-->'; 'asserts.istrue(true);'});

runtimeOptions = struct('Timeout', [], 'LauncherGrace', 0, ...
  'WorkerPoolInline', false);
cases = [initialize_test_case(files{1}, '', runtimeOptions); ...
  initialize_test_case(files{2}, '', runtimeOptions)];
cases = annotate_worker_pool_eligibility(cases);
chunks = worker_pool_chunks(cases, 2);
workers = worker_pool_create_workers(chunks, runtimeOptions);
workerFiles = {workers.command_filename};
nativeResults = run_worker_pool_processes(workers, 2);
updated = worker_pool_collect_results(chunks, workers, nativeResults);
updated = worker_pool_retry_missing(updated, 2);
asserts.isequal(length(updated), 2);
asserts.istrue(all(strcmp({updated.status}, 'Pass')));
for k = 1:length(workerFiles)
  asserts.isfalse(isfile(workerFiles{k}));
end

for k = 1:length(files)
  run_removeFile(files{k});
end
%=============================================================================
