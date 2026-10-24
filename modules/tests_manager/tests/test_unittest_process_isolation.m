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
previousForceNativeRunner = getenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER');
cleanupForceNativeRunner = onCleanup(...
  @() setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', previousForceNativeRunner)); %#ok<NLS0004>
setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', 'TRUE');
previousOmpThreads = getenv('OMP_NUM_THREADS');
if isempty(previousOmpThreads)
  restoreOmpThreadsCommand = 'unsetenv(''OMP_NUM_THREADS'');';
else
  escapedOmpThreads = strrep(previousOmpThreads, '''', '''''');
  restoreOmpThreadsCommand = ['setenv(''OMP_NUM_THREADS'', ''', ...
    escapedOmpThreads, ''');'];
end
cleanupOmpThreads = onCleanup(@() evalc(restoreOmpThreadsCommand)); %#ok<NLS0004>
unsetenv('OMP_NUM_THREADS');
global NELSON_UNITTEST_PROCESS_ISOLATION_MARKER; %#ok<NLS0005>
NELSON_UNITTEST_PROCESS_ISOLATION_MARKER = [];

testFile = [[tempdir(), ''], 'test_unittest_process_isolation_', createGUID(), '.m'];
code = [
  'global NELSON_UNITTEST_PROCESS_ISOLATION_MARKER;', newline, ...
  'NELSON_UNITTEST_PROCESS_ISOLATION_MARKER = 42;', newline, ...
  'asserts.istrue(true);', newline];
filewrite(testFile, code);

cleanupFile = onCleanup(@() evalc(removeCommand(testFile))); %#ok<NLS0004>
cleanupGlobal = onCleanup(@() evalc('clear global NELSON_UNITTEST_PROCESS_ISOLATION_MARKER')); %#ok<NLS0004>

output = evalc('status = test_run(testFile);');
asserts.istrue(status);
asserts.istrue(contains(output, 'test_unittest_process_isolation'));
asserts.isequal(NELSON_UNITTEST_PROCESS_ISOLATION_MARKER, []);
asserts.isequal(getenv('OMP_NUM_THREADS'), '');
%=============================================================================
