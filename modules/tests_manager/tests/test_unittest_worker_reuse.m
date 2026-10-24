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
previousForce = getenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER');
cleanupForce = onCleanup(...
  @() setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', previousForce)); %#ok<NLS0004>
setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', 'TRUE');
base = [[tempdir(), ''], 'test_unittest_worker_reuse_', createGUID()];
files = {
  [base, '_test_one.m']
  [base, '_test_two.m']
  [base, '_test_three.m']
  [base, '_test_four.m']
  [base, '_test_clearall.m']
};
cleanupCommand = '';
for k = 1:length(files)
  cleanupCommand = [cleanupCommand, removeCommand(files{k}), newline];
end
cleanupFiles = onCleanup(@() evalc(cleanupCommand)); %#ok<NLS0004>

for k = 1:length(files)
  if k == length(files)
    filewrite(files{k}, {'clear all'; 'asserts.istrue(true);'});
  else
    filewrite(files{k}, {'% <--REUSE PROCESS-->'; 'asserts.istrue(true);'});
  end
end

results = nelson.unittest.run(files, 'Workers', 2);
asserts.istrue(results.status);
asserts.isequal(length(results.cases), 5);
asserts.istrue(isfield(results.summary, 'worker_pool'));
asserts.isequal(results.summary.worker_pool.attempted, 4);
asserts.isequal(results.summary.worker_pool.completed, 4);
asserts.isequal(results.summary.worker_pool.fallback, 0);
asserts.isequal(results.summary.worker_pool.direct, 1);
for k = 1:length(files)
  [~, expectedName, ~] = fileparts(files{k}); %#ok<NLS0004>
  asserts.isequal(results.cases(k).id, expectedName);
  asserts.isequal(results.cases(k).outcome, 'passed');
end
for k = 1:length(files)
  run_removeFile(files{k});
end
%=============================================================================
