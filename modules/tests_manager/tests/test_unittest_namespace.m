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
asserts.istrue(isstruct(suite));
asserts.istrue(suite.count >= 1);
asserts.istrue(isfield(suite, 'cases'));

filtered = nelson.unittest.select(suite, 'Name', 'test_skip_testsuite_condition_false');
asserts.isequal(filtered.count, 1);
asserts.isequal(filtered.cases.name, 'test_skip_testsuite_condition_false');
asserts.isequal(nelson.unittest.select(suite, 'Kind', 'all').count, suite.count);
asserts.isequal(nelson.unittest.select(suite, 'Kind', 'all_tests').count, suite.count);

testPlan = nelson.unittest.plan(filtered, 'Workers', 1, 'ShardIndex', 1, 'ShardCount', 1);
asserts.isequal(testPlan.type, 'TestPlan');
asserts.isequal(testPlan.workers, 1);
asserts.isequal(length(testPlan.cases), 1);
asserts.istrue(isfield(testPlan, 'groups'));
asserts.istrue(isfield(testPlan.groups, 'parallel'));

reportFile = [[tempdir(), ''], 'test_unittest_namespace_', createGUID(), '.json'];
cleanup = onCleanup(@() evalc(removeCommand(reportFile))); %#ok<NLS0004>
status = nelson.unittest.report(testPlan, 'Format', 'json', 'OutputFile', reportFile);
asserts.istrue(status);
asserts.istrue(isfile(reportFile));
%=============================================================================
