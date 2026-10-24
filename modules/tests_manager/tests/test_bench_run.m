%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
base = [[tempdir(), ''], 'test_bench_run_', createGUID()];
benchFile = [fileparts(base, 'path'), '/bench_bench_run_', createGUID(), '.m'];
testFile = [base, '.m'];
markerFile = [base, '.marker'];
cleanupBench = onCleanup(@() evalc(removeCommand(benchFile))); %#ok<NLS0004>
cleanupTest = onCleanup(@() evalc(removeCommand(testFile))); %#ok<NLS0004>
cleanupMarker = onCleanup(@() evalc(removeCommand(markerFile))); %#ok<NLS0004>

filewrite(benchFile, {['filewrite(''', markerFile, ''', ''executed'');']});
filewrite(testFile, {'error(''This test must not be executed.'');'});

[~, ~, ~, defaultOption, ~] = test_run_parse_input_arguments(0, {}); %#ok<NLS0004>
asserts.isequal(defaultOption, 'all_tests');
[~, ~, ~, moduleOption, ~] = test_run_parse_input_arguments(1, {'string'}); %#ok<NLS0004>
asserts.isequal(moduleOption, 'all_tests');
[~, ~, ~, explicitOption, ~] = test_run_parse_input_arguments(1, {'all'}); %#ok<NLS0004>
asserts.isequal(explicitOption, 'all');

output = evalc('status = bench_run({benchFile, testFile}, ''Workers'', 1);');
asserts.istrue(status);
asserts.istrue(contains(output, 'bench_bench_run'));
asserts.isequal(fileread(markerFile), 'executed');
run_removeFile(benchFile);
run_removeFile(testFile);
run_removeFile(markerFile);
%=============================================================================
