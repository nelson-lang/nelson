%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
skip_if_no_native_unittest_runner();
%=============================================================================
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
previousForceNative = getenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER');
cleanupForceNative = onCleanup(...
  @() setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', previousForceNative)); %#ok<NLS0004>
setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', 'TRUE');
base = [[tempdir(), ''], 'test_unittest_worker_pool_mixed_', createGUID()];
files = {
  [base, '_test_one.m']
  [base, '_test_two.m']
  [base, '_test_clearall.m']
  [base, '_test_three.m']
};
cleanupCommand = '';
for k = 1:length(files)
  cleanupCommand = [cleanupCommand, removeCommand(files{k}), newline];
end
cleanupFiles = onCleanup(@() evalc(cleanupCommand)); %#ok<NLS0004>

for k = 1:length(files)
  if k == 3
    filewrite(files{k}, {'clear all'; 'asserts.istrue(true);'});
  else
    filewrite(files{k}, {'% <--REUSE PROCESS-->'; 'asserts.istrue(true);'});
  end
end

results = nelson.unittest.run(files, 'Workers', 2);
asserts.istrue(results.status);
asserts.isequal(length(results.cases), 4);
asserts.isequal(results.summary.worker_pool.attempted, 3);
asserts.isequal(results.summary.worker_pool.completed, 3);
asserts.isequal(results.summary.worker_pool.direct, 1);
asserts.istrue(results.summary.worker_pool.native_jobs >= 2);
asserts.istrue(results.summary.worker_pool.native_jobs <= 3);
asserts.isequal(results.summary.worker_pool.inline_jobs, 0);
asserts.isequal(results.summary.worker_pool.script_jobs, 1);
for k = 1:length(files)
  [~, expectedName, ~] = fileparts(files{k}); %#ok<NLS0004>
  asserts.isequal(results.cases(k).id, expectedName);
  asserts.isequal(results.cases(k).outcome, 'passed');
end

launcherCase.options = parsetags(files{1});
launcherCase = assign_test_case_launcher(launcherCase, '--quiet');
launcherExecutable = lower(launcherCase.executable);
asserts.istrue(contains(launcherExecutable, 'nelson-cli'));
asserts.isfalse(contains(launcherExecutable, 'nelson-gui'));

for k = 1:length(files)
  run_removeFile(files{k});
end
%=============================================================================
