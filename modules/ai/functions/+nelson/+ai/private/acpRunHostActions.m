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
  badParse = 0;
  for k = 1:numel(tokens)
    [spec, reason] = parseActionSpec(tokens{k}{1});
    if isempty(spec)
      % A marker that cannot be decoded (the common cause: an LLM dropped a
      % multi-line "code" value or a Windows path into the inline JSON without
      % escaping it) is reported, never skipped in silence, so the model's
      % optimistic prose is not left standing over an action that never ran. A
      % disallowed or tool-less marker stays silent: it is treated as injected
      % noise, matching the allowlist's intent.
      if strcmp(reason, 'badjson')
        badParse = badParse + 1;
        aiAgentEmit('activity', struct('kind', 'result', 'name', 'action', ...
          'text', 'A requested editor action was skipped: its format was not valid.'), ...
          surfaceId);
      end
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
  % A legitimate-looking action that could not be parsed is called out plainly, so
  % the user is never left with "the code is in the editor" while nothing happened.
  if badParse > 0
    cleaned = appendActionWarning(cleaned);
  end
end
%=============================================================================
function [spec, reason] = parseActionSpec(jsonText)
  spec = [];
  reason = 'badjson';
  % Repair the inline JSON before decoding: an LLM routinely embeds a multi-line
  % "code" value (raw line breaks) or a Windows path (a lone backslash) in the
  % marker, both of which are invalid JSON. sanitizeMarkerJson escapes those inside
  % string literals so the legitimate action runs instead of being silently lost.
  try
    decoded = jsondecode(sanitizeMarkerJson(strtrim(jsonText)));
  catch
    return
  end
  if ~isstruct(decoded) || ~isfield(decoded, 'tool')
    reason = 'notool';
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
    reason = 'disallowed';
    return
  end
  if isfield(decoded, 'arguments') && isstruct(decoded.arguments)
    args = decoded.arguments;
  else
    args = struct();
  end
  spec = struct('tool', tool, 'args', args);
  reason = 'ok';
end
%=============================================================================
% Make the common LLM-mangled marker JSON decodable without altering a well-formed
% one. Only characters INSIDE string literals are rewritten: a raw line break or
% tab becomes its escape (\n, \r, \t), and a backslash that does not begin a valid
% JSON escape is doubled (so a Windows path like d:\demo.m survives). Structural
% text between tokens is untouched. Worst case each input char expands to two, so a
% buffer of 2*n is always large enough.
function out = sanitizeMarkerJson(jsonText)
  src = char(jsonText);
  n = numel(src);
  out = blanks(2 * n + 8);
  m = 0;
  inString = false;
  i = 1;
  validEscape = '"\/bfnrtu';
  while i <= n
    c = src(i);
    if ~inString
      m = m + 1;
      out(m) = c;
      if c == '"'
        inString = true;
      end
      i = i + 1;
    elseif c == '"'
      m = m + 1;
      out(m) = c;
      inString = false;
      i = i + 1;
    elseif c == '\'
      if i < n && any(src(i + 1) == validEscape)
        out(m + 1) = c;
        out(m + 2) = src(i + 1);
        m = m + 2;
        i = i + 2;
      else
        out(m + 1) = '\';
        out(m + 2) = '\';
        m = m + 2;
        i = i + 1;
      end
    elseif c == char(10)
      out(m + 1) = '\';
      out(m + 2) = 'n';
      m = m + 2;
      i = i + 1;
    elseif c == char(13)
      out(m + 1) = '\';
      out(m + 2) = 'r';
      m = m + 2;
      i = i + 1;
    elseif c == char(9)
      out(m + 1) = '\';
      out(m + 2) = 't';
      m = m + 2;
      i = i + 1;
    else
      m = m + 1;
      out(m) = c;
      i = i + 1;
    end
  end
  out = out(1:m);
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
% Append an honest correction when a requested action could not be parsed, so the
% user is told plainly that nothing was opened or changed. Kept separate from the
% model's own prose (which may wrongly claim success) rather than replacing it.
function out = appendActionWarning(cleaned)
  note = ['Note: a requested editor action could not be carried out (the action ', ...
    'was malformed), so no file was opened or changed.'];
  if isempty(cleaned)
    out = note;
  else
    out = [cleaned, char(10), char(10), note];
  end
end
%=============================================================================
