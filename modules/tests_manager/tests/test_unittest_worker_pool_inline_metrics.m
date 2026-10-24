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
tmpBase = [[tempdir(), ''], 'test_unittest_worker_pool_inline_metrics_', createGUID()];
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
  'WorkerPoolInline', true);
cases = [initialize_test_case(files{1}, '', runtimeOptions); ...
  initialize_test_case(files{2}, '', runtimeOptions)];
cases = annotate_worker_pool_eligibility(cases);
updated = process_test_cases_pooled_raw(cases, 2, runtimeOptions);
metrics = worker_pool_metrics_from_cases(updated);
asserts.isequal(metrics.script_jobs, 0);
asserts.isequal(metrics.inline_jobs, 2);

for k = 1:length(files)
  run_removeFile(files{k});
end
%=============================================================================
