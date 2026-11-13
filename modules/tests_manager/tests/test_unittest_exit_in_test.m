%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
% A test calling exit(N, 'force') must end its process with code N, not fail
% on an unassigned runfile output.
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
base = [[tempdir(), ''], 'test_unittest_exit_in_test_', createGUID()];
files = {[base, '_own_process.m']; [base, '_pooled.m']; [base, '_after.m']};
cleanupCommand = '';
for k = 1:length(files)
  cleanupCommand = [cleanupCommand, removeCommand(files{k}), newline];
end
cleanupFiles = onCleanup(@() evalc(cleanupCommand)); %#ok<NLS0004>
filewrite(files{1}, {'% <--ENGLISH IMPOSED-->'; ...
  'fprintf(2, ''child stderr text\n''); exit(1, ''force'');'});
filewrite(files{2}, {'exit(3, ''force'');'});
filewrite(files{3}, {'asserts.istrue(true);'});
% Nested runs are inline by default: exit would end this process.
previousForceNativeRunner = getenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER');
cleanupNative = onCleanup(@() ...
  setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', previousForceNativeRunner)); %#ok<NLS0004>
setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', 'TRUE');
%=============================================================================
results = nelson.unittest.run(files, 'Workers', 1);
asserts.isequal(numel(results.cases), 3);
asserts.isequal(results.cases(1).outcome, 'failed');
% the pooled worker died: its case is reported as an error
asserts.isequal(results.cases(2).outcome, 'error');
asserts.isequal(results.cases(3).outcome, 'passed');
asserts.isfalse(contains(results.cases(1).message, 'struct_res'));
asserts.isfalse(contains(results.cases(2).message, 'struct_res'));
asserts.istrue(contains(results.cases(1).message, 'exit code 1'));
asserts.istrue(contains(results.cases(2).message, 'exit code 3'));
%=============================================================================
