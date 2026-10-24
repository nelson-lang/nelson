%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function lines = report_htmlRunInfo(results)
  suite = report_htmlFirstSuite(results);
  items = {
    'Run timestamp', report_htmlRunTimestamp(results, suite)
    'Report generated', run_timestamp()
    'Environment', report_htmlEnvironment(suite)
    'Host', report_htmlHost(suite)
    'Architecture', report_htmlArchitecture()
    'Nelson mode', report_htmlNelsonMode()
    'Workers', report_htmlWorkers(results)
    'Shard', report_htmlShard(results)
    'Process policy', report_htmlProcessPolicy(results)
    'Nelson root', nelsonroot()
  };
  lines = {
    '<section class="run-info" aria-label="Environment">'
    '<h2>Environment</h2>'
    '<dl class="info-grid">'
  };
  for k = 1:size(items, 1)
    value = items{k, 2};
    if ~isempty(value)
      lines{end + 1} = ['<div><dt>', report_htmlEscape(items{k, 1}), ...
        '</dt><dd>', report_htmlEscape(value), '</dd></div>'];
    end
  end
  lines{end + 1} = '</dl>';
  lines{end + 1} = '</section>';
  lines = lines(:);
end
%=============================================================================
function suite = report_htmlFirstSuite(results)
  suite = struct();
  raw = results;
  if isfield(results, 'raw')
    raw = results.raw;
  end
  if isfield(raw, 'tests_list') && ~isempty(raw.tests_list)
    suites = raw.tests_list;
    suite = suites(1);
  end
end
%=============================================================================
function value = report_htmlRunTimestamp(results, suite)
  value = '';
  if isfield(suite, 'timestamp')
    value = suite.timestamp;
  elseif isfield(results, 'registry') && isfield(results.registry, 'created_at')
    value = results.registry.created_at;
  end
  if isempty(value)
    value = run_timestamp();
  end
end
%=============================================================================
function value = report_htmlEnvironment(suite)
  value = '';
  if isfield(suite, 'environment')
    value = suite.environment;
  end
  if isempty(value)
    value = get_environment_test();
  end
end
%=============================================================================
function value = report_htmlHost(suite)
  value = '';
  if isfield(suite, 'hostname')
    value = suite.hostname;
  end
  if isempty(value)
    value = hostname();
  end
end
%=============================================================================
function value = report_htmlArchitecture()
  value = '';
  try
    value = char(computer('arch'));
  end
end
%=============================================================================
function value = report_htmlNelsonMode()
  value = '';
  try
    value = getnelsonmode();
  end
end
%=============================================================================
function value = report_htmlWorkers(results)
  value = '';
  if isfield(results, 'plan') && isfield(results.plan, 'workers')
    value = int2str(results.plan.workers);
  elseif isfield(results, 'registry') && isfield(results.registry, 'workers')
    value = int2str(results.registry.workers);
  end
end
%=============================================================================
function value = report_htmlShard(results)
  value = '';
  if isfield(results, 'plan') && isfield(results.plan, 'shard')
    shard = results.plan.shard;
    if length(shard) == 2
      value = [int2str(shard(1)), '/', int2str(shard(2))];
    end
  end
end
%=============================================================================
function value = report_htmlProcessPolicy(results)
  value = '';
  if isfield(results, 'registry') && isfield(results.registry, 'process_policy')
    value = results.registry.process_policy;
  elseif isfield(results, 'plan') && isfield(results.plan, 'resource_policy')
    value = results.plan.resource_policy;
  end
end
%=============================================================================
