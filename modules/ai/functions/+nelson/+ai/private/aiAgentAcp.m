%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Answer a prompt through the claude-agent-acp backend, reusing a single ACP
% session across calls so the conversation keeps its context. The session (a
% persistent subprocess) is reconnected if it has exited. options.Images (base64
% PNGs) are sent to the vision-capable agent.
function reply = aiAgentAcp(prompt, options)
  persistent client
  command = 'claude-agent-acp';
  images = {};
  if nargin >= 2 && isstruct(options) && isfield(options, 'Images') && ~isempty(options.Images)
    images = options.Images;
  end
  if isempty(client) || ~isAcpSessionAlive(client)
    client = nelson.ai.acp.internal.connect(command);
  end
  nelson.ai.internal.aiAgentEmit('reset', struct());
  try
    [reply, client] = nelson.ai.acp.internal.prompt(client, prompt, @emitAcpToken, images);
  catch e
    % A dead session must not be reused on the next prompt.
    client = [];
    if contains(e.message, 'Authentication required')
      error('Nelson:ai:acpAuthRequired', ['Claude authentication is required. ', ...
        'Sign in to Claude Code once (run "claude" in a terminal and log in), then retry.']);
    end
    rethrow(e);
  end
end
%=============================================================================
function tf = isAcpSessionAlive(client)
  tf = isstruct(client) && isfield(client, 'proc') && isvalid(client.proc);
end
%=============================================================================
% Stream one ACP update fragment to the assistant pane as it arrives.
function emitAcpToken(note)
  if ~isstruct(note) || ~isfield(note, 'params') || ~isfield(note.params, 'update')
    return
  end
  fragment = acpUpdateText(note.params.update);
  if ~isempty(fragment)
    nelson.ai.internal.aiAgentEmit('token', struct('text', fragment));
  end
end
%=============================================================================
% Extract the text of an ACP session/update, tolerating both a plain-string
% content and the structured {type:'text', text:...} form.
function out = acpUpdateText(update)
  out = '';
  if ~isstruct(update) || ~isfield(update, 'content')
    return
  end
  content = update.content;
  if ischar(content) || (isstring(content) && isscalar(content))
    out = char(content);
  elseif isstruct(content) && isscalar(content) && isfield(content, 'text')
    out = char(content.text);
  end
end
%=============================================================================
