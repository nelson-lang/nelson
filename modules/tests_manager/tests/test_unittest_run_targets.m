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
testFile = [nelsonroot(), '/modules/tests_manager/tests/test_skip_testsuite_condition_false.m'];
jsonFile = [[tempdir(), ''], 'test_unittest_run_targets_', createGUID(), '.json'];
cleanupJson = onCleanup(@() evalc(removeCommand(jsonFile))); %#ok<NLS0004>

results = nelson.unittest.run(testFile, 'Kind', 'test', 'Workers', 1, ...
  'Format', 'json', 'OutputFile', jsonFile);
asserts.isequal(results.type, 'TestRunResult');
asserts.istrue(results.status);
asserts.isequal(results.summary.failed, 0);
asserts.isequal(length(results.plan.cases), 1);
asserts.istrue(isfile(jsonFile));

output = evalc('legacyStatus = test_run(testFile);');
asserts.istrue(islogical(legacyStatus));
asserts.istrue(legacyStatus);
asserts.istrue(contains(output, 'test_skip_testsuite_condition_false'));

asserts.checkerror('nelson.unittest.run(testFile, ''Kind'')', _('Option value expected.'));
asserts.checkerror('nelson.unittest.discover(testFile, ''Kind'')', _('Option value expected.'));
asserts.checkerror('nelson.unittest.select(nelson.unittest.discover(testFile), ''Name'')', _('Option value expected.'));
asserts.checkerror('nelson.unittest.plan(nelson.unittest.discover(testFile), ''Workers'')', _('Option value expected.'));
%=============================================================================
