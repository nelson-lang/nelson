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
base = [[tempdir(), ''], 'test_unittest_plan_options_', createGUID()];
files = {
  [base, '_test_one.m']
  [base, '_test_two.m']
  [base, '_test_three.m']
  [base, '_test_four.m']
};
cleanup1 = onCleanup(@() evalc(removeCommand(files{1}))); %#ok<NLS0004>
cleanup2 = onCleanup(@() evalc(removeCommand(files{2}))); %#ok<NLS0004>
cleanup3 = onCleanup(@() evalc(removeCommand(files{3}))); %#ok<NLS0004>
cleanup4 = onCleanup(@() evalc(removeCommand(files{4}))); %#ok<NLS0004>
for k = 1:length(files)
  filewrite(files{k}, 'asserts.istrue(true);');
end

suite = nelson.unittest.discover(files, 'Kind', 'test');
plan1 = nelson.unittest.plan(suite, 'Workers', 2, 'ShardIndex', 1, 'ShardCount', 2);
plan2 = nelson.unittest.plan(suite, 'Workers', 2, 'ShardIndex', 2, 'ShardCount', 2);
asserts.isequal(length(plan1.cases), 2);
asserts.isequal(length(plan2.cases), 2);
asserts.isequal(plan1.shard, [1, 2]);
asserts.isequal(plan2.shard, [2, 2]);

shuffled1 = nelson.unittest.plan(suite, 'Shuffle', true, 'Seed', 42);
shuffled2 = nelson.unittest.plan(suite, 'Shuffle', true, 'Seed', 42);
asserts.isequal({shuffled1.cases.name}, {shuffled2.cases.name});
asserts.checkerror(['nelson.unittest.plan(suite, ''ShardIndex'', 3, ', ...
  '''ShardCount'', 2)'], _('Invalid shard configuration.'));
%=============================================================================
