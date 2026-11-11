%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function testPlan = plan(targets, varargin)
  options = plan_parsePlanOptions(varargin);
  plan_validateShard(options.ShardIndex, options.ShardCount);
  suite = targets;
  if ~run_isModernInput(targets)
    % Module names, files or folders: discover them as nelson.unittest.run does.
    suite = nelson.unittest.discover(targets);
  elseif strcmp(targets.type, 'TestPlan')
    error('Nelson:tests_manager:testSuiteExpected', 'A test suite or test targets are expected.');
  end
  cases = suite.cases;
  if options.Shuffle
    rng(options.Seed);
    cases = cases(randperm(length(cases)));
  end
  cases = plan_applyShard(cases, options.ShardIndex, options.ShardCount);
  testPlan = struct();
  testPlan.type = 'TestPlan';
  testPlan.cases = cases;
  testPlan.workers = options.Workers;
  testPlan.shard = [options.ShardIndex, options.ShardCount];
  testPlan.order = 'parallel-first';
  testPlan.resource_policy = 'process-group';
  testPlan.groups = plan_groupCases(cases);
  testPlan.filtered = plan_suiteFilteredCount(suite);
end
