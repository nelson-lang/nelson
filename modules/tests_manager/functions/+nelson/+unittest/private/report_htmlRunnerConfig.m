%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function lines = report_htmlRunnerConfig(results)
  items = {
    'Workers', report_htmlConfigWorkers(results)
    'Mode', report_htmlConfigMode()
    'Shard', report_htmlConfigShard(results)
    'Policy', report_htmlConfigPolicy(results)
  };
  lines = {'<section class="config-strip" aria-label="Runner configuration">'};
  for k = 1:size(items, 1)
    if ~isempty(items{k, 2})
      lines{end + 1} = ['<span><strong>', report_htmlEscape(items{k, 1}), ...
        '</strong> ', report_htmlEscape(items{k, 2}), '</span>'];
    end
  end
  lines{end + 1} = '</section>';
  lines = lines(:);
end
%=============================================================================
function value = report_htmlConfigWorkers(results)
  value = '';
  if isfield(results, 'plan') && isfield(results.plan, 'workers')
    value = int2str(results.plan.workers);
  elseif isfield(results, 'registry') && isfield(results.registry, 'workers')
    value = int2str(results.registry.workers);
  end
end
%=============================================================================
function value = report_htmlConfigMode()
  value = '';
  try
    value = getnelsonmode();
  end
end
%=============================================================================
function value = report_htmlConfigShard(results)
  value = '';
  if isfield(results, 'plan') && isfield(results.plan, 'shard')
    shard = results.plan.shard;
    if length(shard) == 2
      value = [int2str(shard(1)), '/', int2str(shard(2))];
    end
  end
end
%=============================================================================
function value = report_htmlConfigPolicy(results)
  value = '';
  if isfield(results, 'registry') && isfield(results.registry, 'process_policy')
    value = results.registry.process_policy;
  elseif isfield(results, 'plan') && isfield(results.plan, 'resource_policy')
    value = results.plan.resource_policy;
  end
end
%=============================================================================
