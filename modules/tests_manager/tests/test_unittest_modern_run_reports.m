%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
suite = nelson.unittest.discover('tests_manager', 'Kind', 'test');
suite = nelson.unittest.select(suite, 'Name', 'test_skip_testsuite_condition_false');
testPlan = nelson.unittest.plan(suite, 'Workers', 1);

base = [[tempdir(), ''], 'test_unittest_modern_run_reports_', createGUID()];
jsonFile = [base, '.json'];
junitFile = [base, '.xml'];
tapFile = [base, '.tap'];
htmlFile = [base, '.html'];
logDir = [base, '_logs'];
cleanupJson = onCleanup(@() evalc(removeCommand(jsonFile))); %#ok<NLS0004>
cleanupJUnit = onCleanup(@() evalc(removeCommand(junitFile))); %#ok<NLS0004>
cleanupTap = onCleanup(@() evalc(removeCommand(tapFile))); %#ok<NLS0004>
cleanupHtml = onCleanup(@() evalc(removeCommand(htmlFile))); %#ok<NLS0004>

rawPattern = [tempdir(), 'nelson_unittest_raw_*.json'];
rawCountBefore = length(dir(rawPattern));
results = nelson.unittest.run(testPlan, 'Format', 'json', 'OutputFile', jsonFile, ...
  'LogDir', logDir);
rawCountAfter = length(dir(rawPattern));
asserts.isequal(results.type, 'TestRunResult');
asserts.istrue(results.status);
asserts.isequal(results.summary.failed, 0);
asserts.isequal(results.registry.state, 'running');
asserts.isequal(results.registry.process_policy, 'process-group');
asserts.isequal(results.registry.output_file, '');
asserts.isfalse(isfile(results.registry.registry_file));
asserts.isequal(rawCountAfter, rawCountBefore);
asserts.istrue(isfile(jsonFile));
asserts.istrue(isdir(logDir));

asserts.istrue(nelson.unittest.report(results, 'Format', 'junit', 'OutputFile', junitFile));
asserts.istrue(nelson.unittest.report(results, 'Format', 'tap', 'OutputFile', tapFile));
asserts.istrue(nelson.unittest.report(results, 'Format', 'html', 'OutputFile', htmlFile));
asserts.istrue(contains(fileread(junitFile), '<testsuites'));
asserts.istrue(contains(fileread(tapFile), 'TAP version 13'));
asserts.istrue(contains(fileread(htmlFile), 'Nelson Test Report'));
rmdir(logDir, 's');
%=============================================================================
