%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Describe an ACP session/update as one assistant activity line so the pane shows
% what the agent is actually doing (reading/writing a file, running a tool) instead
% of only "thinking". Returns a {kind:'call', name:<label>} struct for a tool call,
% or [] for anything else (plain text chunks are streamed as tokens, not activity).
function act = acpUpdateActivity(update)
  act = [];
  if ~isstruct(update) || ~isscalar(update) || ~isfield(update, 'sessionUpdate')
    return
  end
  % Only the initial tool_call announces an action; status updates would repeat it.
  if ~strcmp(char(update.sessionUpdate), 'tool_call')
    return
  end
  label = toolCallLabel(update);
  if ~isempty(label)
    act = struct('kind', 'call', 'name', label);
  end
end
%=============================================================================
% A human-readable label for the tool call: the agent's own title when present,
% otherwise the tool kind and the file path it touches.
function label = toolCallLabel(update)
  label = '';
  if isfield(update, 'title') && ~isempty(update.title)
    label = char(update.title);
    return
  end
  kindText = '';
  if isfield(update, 'kind')
    kindText = char(update.kind);
  end
  pathText = toolCallPath(update);
  if ~isempty(kindText) && ~isempty(pathText)
    label = [kindText, ' ', pathText];
  elseif ~isempty(kindText)
    label = kindText;
  elseif ~isempty(pathText)
    label = pathText;
  end
end
%=============================================================================
% Best-effort file path of a tool call: the rawInput path, else the first of the
% reported locations. The shape varies, so every access is guarded.
function p = toolCallPath(update)
  p = '';
  if isfield(update, 'rawInput') && isstruct(update.rawInput) && isfield(update.rawInput, 'path')
    p = char(update.rawInput.path);
    if ~isempty(p)
      return
    end
  end
  if isfield(update, 'locations') && ~isempty(update.locations)
    locs = update.locations;
    if iscell(locs)
      first = locs{1};
    else
      first = locs(1);
    end
    if isstruct(first) && isfield(first, 'path')
      p = char(first.path);
    end
  end
end
%=============================================================================
