%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function plan = run_ensurePlan(value, options)
  if run_isModernInput(value) && strcmp(value.type, 'TestPlan')
    plan = value;
  elseif run_isModernInput(value) && strcmp(value.type, 'TestSuite')
    suite = nelson.unittest.select(value, 'Name', options.Name, 'Module', options.Module, ...
      'File', options.File, 'Kind', options.Kind, 'Tags', options.Tags, ...
      'ExcludeTags', options.ExcludeTags, 'Match', options.Match, 'Exclude', options.Exclude);
    plan = nelson.unittest.plan(suite, 'Workers', options.Workers, ...
      'Shuffle', options.Shuffle, 'Seed', options.Seed, ...
      'ShardIndex', options.ShardIndex, 'ShardCount', options.ShardCount);
  else
    suite = nelson.unittest.discover(value, 'Kind', options.Kind);
    plan = run_ensurePlan(suite, options);
  end
end
