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
  userPrompt = char(prompt);
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
  % aiComposePrompt layers identity, verified API, skills, and (ACP) the
  % host-action bridge around the user's text; the pane still shows only the text.
  if strcmpi(model, 'claude-agent-acp')
    % Seed the prompt with recent history ONLY when this surface's ACP session is
    % fresh (no live agent yet): then the agent starts with the conversation the user
    % sees - one continued from the other surface (Qt/web share history) or from before
    % a restart. A session that is already live remembers its own turns, so it is NOT
    % re-seeded. Compose BEFORE recording the user turn, so the seeded history holds
    % only the PRIOR turns; the assistant turn is recorded in acpAsyncStep.
    seedHistory = acpSessionIsFresh(surfaceId);
    augmentedPrompt = aiComposePrompt(userPrompt, surfaceId, true, seedHistory);
    nelson.ai.internal.history.append(surfaceId, 'user', userPrompt);
    startAcpAsync(augmentedPrompt, ~strcmpi(char(driver), 'external'), surfaceId);
    return
  end
  % Compose BEFORE recording the user turn so the injected history holds only the
  % PRIOR turns (the current prompt is already the clearly delimited user request).
  augmentedPrompt = aiComposePrompt(userPrompt, surfaceId, false);
  nelson.ai.internal.history.append(surfaceId, 'user', userPrompt);
  if isempty(model)
    options = nelson.ai.options('Timeout', 90, 'MaxSteps', 6, 'Stream', true);
  else
    options = nelson.ai.options('Model', model, 'Timeout', 90, 'MaxSteps', 6, 'Stream', true);
  end
  % Route live progress (tool-call activity, streamed tokens) to THIS surface, so
  % the direct backend's feed reaches the right pane like the ACP backend's does.
  options.SurfaceId = surfaceId;
  try
    reply = nelson.ai.agent(augmentedPrompt, options);
    nelson.ai.internal.history.append(surfaceId, 'assistant', char(reply));
    __ai_notify__('answer', jsonencode(struct('text', char(reply))), surfaceId);
  catch e
    __ai_notify__('error', jsonencode(struct('text', char(e.message))), surfaceId);
  end
end
%=============================================================================
% True when this surface has no live ACP agent yet (first prompt, or the previous
% agent process is gone): the composed prompt then seeds the agent with the shared
% history. Mirrors startAcpAsync's own reuse test.
function tf = acpSessionIsFresh(surfaceId)
  client = acpAsyncState('get', surfaceId);
  tf = isempty(client) ...
    || ~(isstruct(client) && isfield(client, 'proc') && isvalid(client.proc));
end
%=============================================================================
