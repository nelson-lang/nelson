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
% <--SEQUENTIAL TEST REQUIRED-->
skip_if_no_native_unittest_runner();
%=============================================================================
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
previousForceNativeRunner = getenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER');
cleanupForceNativeRunner = onCleanup(...
  @() setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', previousForceNativeRunner)); %#ok<NLS0004>
setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', 'TRUE');
tempRoot = [tempdir(), ''];
base = [tempRoot, 'test_unittest_bench_sequential_', createGUID()];
benchOne = [tempRoot, 'bench_unittest_sequential_one_', createGUID(), '.m'];
benchTwo = [tempRoot, 'bench_unittest_sequential_two_', createGUID(), '.m'];
markerFile = [base, '.order'];
startOne = [base, '.start_one'];
endOne = [base, '.end_one'];
startTwo = [base, '.start_two'];
endTwo = [base, '.end_two'];
cleanupOne = onCleanup(@() evalc(removeCommand(benchOne))); %#ok<NLS0004>
cleanupTwo = onCleanup(@() evalc(removeCommand(benchTwo))); %#ok<NLS0004>
cleanupMarker = onCleanup(@() evalc(removeCommand(markerFile))); %#ok<NLS0004>
cleanupStartOne = onCleanup(@() evalc(removeCommand(startOne))); %#ok<NLS0004>
cleanupEndOne = onCleanup(@() evalc(removeCommand(endOne))); %#ok<NLS0004>
cleanupStartTwo = onCleanup(@() evalc(removeCommand(startTwo))); %#ok<NLS0004>
cleanupEndTwo = onCleanup(@() evalc(removeCommand(endTwo))); %#ok<NLS0004>

codeOne = {
  ['filewrite(''', startOne, ''', sprintf(''%.17g'', time()));']
  ['previous = ''''; if isfile(''', markerFile, '''), previous = fileread(''', markerFile, '''), end;']
  'pause(1);'
  ['filewrite(''', markerFile, ''', [previous, ''one'', newline]);']
  ['filewrite(''', endOne, ''', sprintf(''%.17g'', time()));']
};
codeTwo = {
  ['filewrite(''', startTwo, ''', sprintf(''%.17g'', time()));']
  ['previous = ''''; if isfile(''', markerFile, '''), previous = fileread(''', markerFile, '''), end;']
  'pause(1);'
  ['filewrite(''', markerFile, ''', [previous, ''two'', newline]);']
  ['filewrite(''', endTwo, ''', sprintf(''%.17g'', time()));']
};
filewrite(benchOne, codeOne);
filewrite(benchTwo, codeTwo);

results = nelson.unittest.run({benchOne; benchTwo}, 'Kind', 'bench', ...
  'Workers', 5);
asserts.istrue(results.status);
asserts.isequal(results.summary.bench, 2);
asserts.isequal(results.summary.tests, 0);
asserts.isequal(fileread(markerFile), ['one', newline, 'two', newline]);
serialOverlap = max(str2double(fileread(startOne)), str2double(fileread(startTwo))) ...
  < min(str2double(fileread(endOne)), str2double(fileread(endTwo)));
asserts.isfalse(serialOverlap);

run_removeFile(markerFile);
run_removeFile(startOne);
run_removeFile(endOne);
run_removeFile(startTwo);
run_removeFile(endTwo);
results = nelson.unittest.run({benchOne; benchTwo}, 'Kind', 'bench', ...
  'Workers', 6);
asserts.istrue(results.status);
parallelOverlap = max(str2double(fileread(startOne)), str2double(fileread(startTwo))) ...
  < min(str2double(fileread(endOne)), str2double(fileread(endTwo)));
asserts.istrue(parallelOverlap);
asserts.isequal(benchmark_worker_count(5), 1);
asserts.isequal(benchmark_worker_count(6), 2);
%=============================================================================
