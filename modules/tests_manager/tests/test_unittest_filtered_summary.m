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
base = [[tempdir(), ''], 'test_unittest_filtered_summary_', createGUID()];
keepFile = [base, '_test_keep.m'];
dropFile = [base, '_test_drop.m'];
cleanupKeep = onCleanup(@() evalc(removeCommand(keepFile))); %#ok<NLS0004>
cleanupDrop = onCleanup(@() evalc(removeCommand(dropFile))); %#ok<NLS0004>

filewrite(keepFile, {'% <--ENGLISH IMPOSED-->'; 'asserts.istrue(true);'});
filewrite(dropFile, {'asserts.istrue(true);'});

suite = nelson.unittest.discover({keepFile; dropFile}, 'Kind', 'test');
selected = nelson.unittest.select(suite, 'Match', 'keep');
asserts.isequal(length(selected.cases), 1);
asserts.isequal(selected.filtered, 1);
asserts.isequal(nelson.unittest.select(suite, 'Tags', 'english_imposed').count, 1);
asserts.isequal(nelson.unittest.select(suite, 'ExcludeTags', 'english_imposed').count, 1);
asserts.isequal(nelson.unittest.select(suite, 'File', keepFile).count, 1);
asserts.isequal(nelson.unittest.select(suite, 'Exclude', 'drop').count, 1);

testPlan = nelson.unittest.plan(selected, 'Workers', 1);
asserts.isequal(testPlan.filtered, 1);

results = nelson.unittest.run(suite, 'Match', 'keep', 'Workers', 1);
asserts.istrue(results.status);
asserts.isequal(results.summary.filtered, 1);
asserts.isequal(length(results.cases), 1);
%=============================================================================
