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
function [cleaned, failureText, dataText] = acpRunHostActions(text, surfaceId)
  narginchk(2, 2);
  nargoutchk(0, 3);
  cleaned = char(text);
  failureText = '';
  dataText = '';
  surfaceId = char(surfaceId);
  pattern = '<<nelson-action>>([\s\S]*?)<<end>>';
  tokens = regexp(cleaned, pattern, 'tokens');
  if isempty(tokens)
    return
  end
  done = {};
  badParse = 0;
  failures = {};
  dataResults = {};
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
        nelson.ai.internal.aiAgentEmit('activity', struct('kind', 'result', 'name', 'action', ...
          'text', 'A requested editor action was skipped: its format was not valid.'), ...
          surfaceId);
      end
      continue
    end
    [ok, note] = runOneAction(spec.tool, spec.args, surfaceId);
    if ok
      done{end + 1} = spec.tool; %#ok<AGROW>
      % A data-producing action (profiling) returns a result the agent must reason
      % about, not just a confirmation; keep it so acpAsyncStep can feed it back.
      if isDataTool(spec.tool) && ~isempty(note)
        dataResults{end + 1} = char(note); %#ok<AGROW>
      end
    else
      % The action parsed and ran but the host could not carry it out (no editor
      % surface, missing file, ...). Keep its reason so the reply is corrected,
      % not just logged as an activity line.
      failures{end + 1} = note; %#ok<AGROW>
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
  % An action that ran but failed is corrected in the displayed reply, so the
  % model's optimistic prose ("the code is in the editor") never stands over a
  % host action that did not actually happen. The joined reasons are also returned
  % so the caller (acpAsyncStep) can feed them back to the agent for a retry.
  if ~isempty(failures)
    reasons = dedupReasons(failures);
    failureText = strjoin(reasons, ' ');
    cleaned = appendActionFailure(cleaned, reasons);
  end
  % Results of data-producing actions, for the caller to feed back to the agent so
  % it reasons about them (e.g. analyzes the profile) instead of only opening a panel.
  if ~isempty(dataResults)
    dataText = strjoin(dataResults, [char(10), char(10)]);
  end
end
%=============================================================================
% A host action whose successful output is DATA the agent should analyze (not just
% an "opened/ran it" confirmation).
function tf = isDataTool(tool)
  tf = strcmp(tool, 'profile_current_file');
end
%=============================================================================
function reasons = dedupReasons(failures)
  reasons = {};
  for k = 1:numel(failures)
    r = strtrim(char(failures{k}));
    if ~isempty(r) && ~any(strcmp(r, reasons))
      reasons{end + 1} = r; %#ok<AGROW>
    end
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
    'insert_into_nelson_file', 'open_nelson_doc', 'run_current_file', ...
    'profile_current_file', 'debug_nelson_file', 'clear_breakpoints', ...
    'propose_nelson_edit'};
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
function [ok, note] = runOneAction(toolName, args, surfaceId)
  ok = false;
  note = '';
  nelson.ai.internal.aiAgentEmit('activity', struct('kind', 'call', 'name', toolName), surfaceId);
  try
    result = nelson.ai.mcp.call(toolName, args);
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
    nelson.ai.internal.aiAgentEmit('activity', ...
      struct('kind', 'result', 'name', toolName, 'text', char(note)), surfaceId);
  catch e
    note = char(e.message);
    nelson.ai.internal.aiAgentEmit('activity', ...
      struct('kind', 'result', 'name', toolName, 'text', note), surfaceId);
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
    case 'open_nelson_doc'
      s = 'Opened the documentation.';
    case 'run_current_file'
      s = 'Ran the file in the console.';
    case 'profile_current_file'
      s = 'Profiled the file and summarized its hot spots.';
    case 'debug_nelson_file'
      s = 'Set a breakpoint and opened the file for debugging.';
    case 'clear_breakpoints'
      s = 'Cleared breakpoints.';
    case 'propose_nelson_edit'
      s = 'Opened the proposed changes in the editor for you to review and save.';
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
% Append an honest correction when an action ran but the host could not complete
% it (no editor surface, missing file, ...). Reasons are collected from the tool
% results so the user learns what actually failed, separate from the model's prose.
function out = appendActionFailure(cleaned, reasons)
  if isempty(reasons)
    detail = 'a requested editor action could not be completed';
  else
    detail = strjoin(reasons, ' ');
  end
  note = ['Note: ', detail, ' No file was opened or changed.'];
  if isempty(cleaned)
    out = note;
  else
    out = [cleaned, char(10), char(10), note];
  end
end
%=============================================================================
