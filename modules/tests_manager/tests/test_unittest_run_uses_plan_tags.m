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
testFile = [[tempdir(), ''], 'test_unittest_run_uses_plan_tags_', createGUID(), '.m'];
cleanupFile = onCleanup(@() evalc(removeCommand(testFile))); %#ok<NLS0004>

filewrite(testFile, {'asserts.istrue(true);'});
suite = nelson.unittest.discover(testFile);
filewrite(testFile, {'% <--INTERACTIVE TEST-->'; 'asserts.istrue(true);'});

results = nelson.unittest.run(suite, 'Workers', 1);
asserts.istrue(results.status);
asserts.isequal(length(results.cases), 1);
asserts.isequal(results.cases(1).outcome, 'passed');
%=============================================================================
