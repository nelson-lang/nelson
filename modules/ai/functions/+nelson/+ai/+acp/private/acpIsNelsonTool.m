%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% True when a pending permission is for one of Nelson's own READ-ONLY MCP lookup
% tools. That server is started read-only (execution and format disabled in
% nelson.ai.acp.connect), so these calls cannot mutate the session and are safe to
% auto-allow -- which is what stops the agent stalling on an unanswered grounding
% lookup. Matching is on an explicit allowlist (not merely the word 'nelson') so a
% file edit whose path happens to contain 'nelson' is NOT swept in: it keeps its
% normal risk handling, as do the agent's built-in file/shell tools.
function tf = acpIsNelsonTool(perm)
  narginchk(1, 1);
  nargoutchk(0, 1);
  tf = false;
  if ~isstruct(perm)
    return
  end
  readOnlyTools = {'find_nelson_symbol', 'get_nelson_help', ...
    'get_nelson_function_info', 'get_nelson_version', 'get_nelson_workspace', ...
    'detect_nelson_modules', 'check_nelson_code', 'lint_nelson_code', ...
    'get_nelson_version', 'get_nelson_modules'};
  hay = lower([char(getFieldOr(perm, 'tool')), ' ', char(getFieldOr(perm, 'kind'))]);
  % Normalize separators so "find_nelson_symbol", "mcp__nelson__find_nelson_symbol"
  % and a humanized "Find Nelson Symbol" title all match the same phrase.
  normalized = regexprep(hay, '[^a-z0-9]+', ' ');
  for k = 1:numel(readOnlyTools)
    name = readOnlyTools{k};
    if contains(hay, name) || contains(normalized, strrep(name, '_', ' '))
      tf = true;
      return
    end
  end
end
%=============================================================================
function value = getFieldOr(st, field)
  if isfield(st, field)
    value = st.(field);
  else
    value = '';
  end
end
%=============================================================================
