%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
base = [tempdir(), 'test_unittest_plan_targets_', createGUID()];
files = {[base, '_test_one.m']; [base, '_test_two.m']};
for k = 1:length(files)
  filewrite(files{k}, 'asserts.istrue(true);');
end
cleanupFiles = onCleanup(@() cellfun(@(f) rmfile(f), files)); %#ok<NLS0004>
%=============================================================================
% Targets are discovered as nelson.unittest.run does.
fromTargets = nelson.unittest.plan(files, 'Workers', 2);
fromSuite = nelson.unittest.plan(nelson.unittest.discover(files), 'Workers', 2);
asserts.isequal(fromTargets.type, 'TestPlan');
asserts.isequal(length(fromTargets.cases), 2);
asserts.isequal(sort({fromTargets.cases.file}), sort({fromSuite.cases.file}));
%=============================================================================
moduleTargets = nelson.unittest.plan({'tests_manager'});
asserts.istrue(length(moduleTargets.cases) > 0);
asserts.istrue(all(strcmp({moduleTargets.cases.module}, 'tests_manager')));
%=============================================================================
% A plan is not a suite.
asserts.checkerror('nelson.unittest.plan(fromTargets)', ...
  _('A test suite or test targets are expected.'), 'Nelson:tests_manager:testSuiteExpected');
%=============================================================================
