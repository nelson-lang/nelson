%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Execute the host-action markers the claude-agent-acp agent left in its reply so
% it can pilot the live GUI (see acpHostActionPreamble). Each
% <<nelson-action>>{json}<<end>> is dispatched through nelson.ai.mcp.call in this
% live session, reported as an activity line, then stripped from the text shown to
% the user. Returns the cleaned reply. Runs in-session, so it needs no MCP server.
function cleaned = acpRunHostActions(text, surfaceId)
  narginchk(2, 2);
  nargoutchk(0, 1);
  cleaned = char(text);
  surfaceId = char(surfaceId);
  pattern = '<<nelson-action>>([\s\S]*?)<<end>>';
  tokens = regexp(cleaned, pattern, 'tokens');
  if isempty(tokens)
    return
  end
  done = {};
  for k = 1:numel(tokens)
    spec = parseActionSpec(tokens{k}{1});
    if isempty(spec)
      continue
    end
    if runOneAction(spec.tool, spec.args, surfaceId)
      done{end + 1} = spec.tool; %#ok<AGROW>
    end
  end
  % Drop the markers from the reply the pane displays.
  cleaned = strtrim(regexprep(cleaned, pattern, ''));
  % An action-only reply (markers, no prose) leaves nothing to show: give a short
  % honest confirmation so the pane never renders an empty "(no response)" bubble.
  if isempty(cleaned) && ~isempty(done)
    cleaned = actionConfirmation(done);
  end
end
%=============================================================================
function spec = parseActionSpec(jsonText)
  spec = [];
  try
    decoded = jsondecode(strtrim(jsonText));
  catch
    return
  end
  if ~isstruct(decoded) || ~isfield(decoded, 'tool')
    return
  end
  % The marker is a NARROW channel for the curated widget actions only, never a
  % backdoor to the full tool set (for example evaluate_nelson_code): a tool name
  % outside the allowlist is ignored, so a marker injected through a file the agent
  % read cannot run arbitrary code here.
  tool = char(decoded.tool);
  allowed = {'open_nelson_file', 'launch_nflow', 'open_nelson_notebook', ...
    'insert_into_nelson_file'};
  if ~any(strcmp(tool, allowed))
    return
  end
  if isfield(decoded, 'arguments') && isstruct(decoded.arguments)
    args = decoded.arguments;
  else
    args = struct();
  end
  spec = struct('tool', tool, 'args', args);
end
%=============================================================================
function ok = runOneAction(toolName, args, surfaceId)
  ok = false;
  aiAgentEmit('activity', struct('kind', 'call', 'name', toolName), surfaceId);
  try
    result = nelson.ai.mcp.call(toolName, args);
    note = '';
    failed = false;
    if isstruct(result)
      if isfield(result, 'success') && ~result.success && isfield(result, 'error')
        note = result.error;
        failed = true;
      elseif isfield(result, 'output')
        note = result.output;
      end
    end
    ok = ~failed;
    aiAgentEmit('activity', ...
      struct('kind', 'result', 'name', toolName, 'text', char(note)), surfaceId);
  catch e
    aiAgentEmit('activity', ...
      struct('kind', 'result', 'name', toolName, 'text', char(e.message)), surfaceId);
  end
end
%=============================================================================
% A one-line, honest acknowledgement of the widget actions that ran, for the rare
% reply that carries only markers. It never claims a file was saved (insert leaves
% an unsaved draft), matching acpHostActionPreamble.
function msg = actionConfirmation(tools)
  parts = {};
  for k = 1:numel(tools)
    line = friendlyAction(tools{k});
    if ~isempty(line) && ~any(strcmp(line, parts))
      parts{end + 1} = line; %#ok<AGROW>
    end
  end
  if isempty(parts)
    msg = 'Done.';
  else
    msg = strjoin(parts, ' ');
  end
end
%=============================================================================
function s = friendlyAction(tool)
  switch tool
    case 'open_nelson_file'
      s = 'Opened the file in the editor.';
    case 'launch_nflow'
      s = 'Opened the NFlow editor.';
    case 'open_nelson_notebook'
      s = 'Opened the notebook.';
    case 'insert_into_nelson_file'
      s = 'Put the code in the editor for you to review and save.';
    otherwise
      s = '';
  end
end
%=============================================================================
