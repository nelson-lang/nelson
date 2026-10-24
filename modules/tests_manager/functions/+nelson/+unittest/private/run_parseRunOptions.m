%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function options = run_parseRunOptions(args)
  options = struct('Workers', maxNumCompThreads(), 'StopOnFail', false, 'OutputFile', '', ...
    'Format', '', 'LogDir', '', 'Shuffle', false, ...
    'Seed', 0, 'ShardIndex', 1, 'ShardCount', 1, ...
    'Retry', 0, 'RetryOnlyOn', 'failed', 'Timeout', [], ...
    'Verbose', false, 'WorkerPoolInline', false, ...
    'Name', [], 'Module', [], 'File', [], 'Kind', 'all', 'Tags', [], ...
    'ExcludeTags', [], 'Match', [], 'Exclude', [], 'Launcher', 'default', ...
    'ExecutionMode', 'auto');
  k = 1;
  while k <= length(args)
    if k == length(args)
      error('Nelson:tests_manager:optionValueExpected', 'Option value expected.');
    end
    name = args{k};
    value = args{k + 1};
    if isfield(options, name)
      options.(name) = value;
    else
      error('Nelson:tests_manager:unsupportedOption', 'Unsupported option.');
    end
    k = k + 2;
  end
end
