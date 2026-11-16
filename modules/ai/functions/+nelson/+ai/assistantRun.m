%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Run the assistant for the web desktop pane and push the answer as an event.
% Queued (not run synchronously in an RPC) so the engine stays interruptible and
% the UI responsive while the model works. Streaming tokens are pushed by the
% agent; this adds a final 'answer' event (or 'error' on failure).
function assistantRun(prompt, model, driver, surfaceId)
  narginchk(1, 4);
  if nargin < 2
    model = '';
  end
  if nargin < 3
    driver = '';
  end
  if nargin < 4
    surfaceId = 'web';
  end
  model = char(model);
  surfaceId = char(surfaceId);
  % Prepend any applicable project skill guide (for example the NFlow model
  % authoring API when the assistant is opened from the editor). The augmented
  % prompt feeds the model; the pane still shows only the user's own text.
  augmentedPrompt = char(prompt);
  preamble = aiSkillContext(augmentedPrompt, surfaceId);
  if ~isempty(preamble)
    augmentedPrompt = [preamble, newline, newline, ...
      'User request:', newline, newline, augmentedPrompt];
  end
  % wasm has no subprocess, so the ACP backend (which spawns claude-agent-acp)
  % cannot run there: tell the user to pick an HTTP provider instead.
  if strcmpi(model, 'claude-agent-acp') && iswasm()
    __ai_notify__('error', jsonencode(struct('text', ...
      ['The Claude (ACP) backend needs a local process and is not available in ', ...
      'the browser engine. Select an HTTP provider (such as a local Ollama or ', ...
      'an OpenAI-compatible server) instead.'])), surfaceId);
    return
  end
  % claude-agent-acp runs asynchronously so the interpreter stays free: the
  % Command Window keeps its prompt and Stop works. driver='external' means the
  % caller drives the poll itself (the Qt dock from its own QTimer); otherwise a
  % Nelson timer drives it (web pane / Command Window). surfaceId isolates the
  % session so concurrent prompts on different surfaces do not collide.
  if strcmpi(model, 'claude-agent-acp')
    % Give the agent the in-session widget-piloting bridge (parsed by the poll in
    % acpRunHostActions); it works without the MCP subprocess.
    augmentedPrompt = [acpHostActionPreamble(), newline, newline, ...
      'User request:', newline, newline, augmentedPrompt];
    startAcpAsync(augmentedPrompt, ~strcmpi(char(driver), 'external'), surfaceId);
    return
  end
  if isempty(model)
    options = nelson.ai.options('Timeout', 90, 'MaxSteps', 6, 'Stream', true);
  else
    options = nelson.ai.options('Model', model, 'Timeout', 90, 'MaxSteps', 6, 'Stream', true);
  end
  try
    reply = nelson.ai.agent(augmentedPrompt, options);
    __ai_notify__('answer', jsonencode(struct('text', char(reply))), surfaceId);
  catch e
    __ai_notify__('error', jsonencode(struct('text', char(e.message))), surfaceId);
  end
end
%=============================================================================
