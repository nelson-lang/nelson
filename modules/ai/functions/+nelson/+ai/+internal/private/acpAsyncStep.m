%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Advance the pending claude-agent-acp request OF ONE SURFACE (surfaceId) by one
% non-blocking step: check that surface's Stop, read whatever the agent produced,
% emit streamed tokens and, once the reply is complete, the final answer. Returns
% true while the request is still running. Each surface has its own client and
% Stop, so stepping one never disturbs another.
function active = acpAsyncStep(surfaceId)
  narginchk(0, 1);
  nargoutchk(0, 1);
  if nargin < 1
    surfaceId = 'web';
  end
  surfaceId = char(surfaceId);
  active = false;
  if __ai_interrupt_pending__(surfaceId)
    disconnectAcpAsync(surfaceId);
    nelson.ai.internal.aiAgentEmit('error', struct('text', '(stopped)'), surfaceId);
    return
  end
  client = acpAsyncState('get', surfaceId);
  if isempty(client)
    return
  end
  % A step may still be posted after the turn ended (the Qt dock polls at a fixed
  % interval, fire-and-forget). Polling an idle client re-emits a blank answer, so
  % the finished turn cascades empty bubbles: no-op once the turn is done.
  if ~isfield(client, 'pending') || ~isstruct(client.pending) || ~client.pending.active
    return
  end
  try
    [done, text, client] = nelson.ai.acp.internal.poll(client, @(note) acpAsyncToken(note, surfaceId));
    acpAsyncState('set', surfaceId, client);
  catch e
    disconnectAcpAsync(surfaceId);
    nelson.ai.internal.aiAgentEmit('error', struct('text', char(e.message)), surfaceId);
    return
  end
  if done
    % Keep the (now idle) client so the next prompt reuses the same agent.
    acpAsyncState('set', surfaceId, client);
    % Run any widget-piloting actions the agent requested, then show the reply
    % without the action markers.
    [text, failureText, dataText] = acpRunHostActions(char(text), surfaceId);
    % A host action the agent requested failed (e.g. the file was not open). Feed
    % the reason back so the agent corrects itself, ONCE per user turn (bounded, so
    % a persistent failure cannot loop). The direct backend gets this for free (the
    % tool result is its observation); the ACP agent's markers are one-shot, so the
    % correction has to be sent as a follow-up prompt on the same session.
    retries = 0;
    if isfield(client, 'hostActionRetries') && ~isempty(client.hostActionRetries)
      retries = client.hostActionRetries;
    end
    if ~isempty(failureText) && retries < 1
      client.hostActionRetries = retries + 1;
      nelson.ai.internal.aiAgentEmit('activity', struct('kind', 'result', 'name', 'action', ...
        'text', ['Retrying after a failed action: ', failureText]), surfaceId);
      corrective = ['A host action you just requested did not work: ', failureText, ...
        ' Fix it and try again (for example save the file first, or pass an existing path), ', ...
        'then confirm in words.'];
      try
        client = nelson.ai.acp.internal.send(client, corrective);
        acpAsyncState('set', surfaceId, client);
        active = true;
        return
      catch
        % Could not re-prompt: fall through and show the reply with its note.
      end
    end
    client.hostActionRetries = 0;
    % A data-producing action (profiling) returned a result the agent did not have
    % when it wrote its reply (markers run AFTER the turn). Feed it back ONCE so the
    % agent actually analyzes it for the user, instead of only pointing at the panel.
    % Bounded like the failure retry, so it cannot loop. The original reply (which
    % could only say "I launched it") is dropped in favour of the analysis.
    dataFeeds = 0;
    if isfield(client, 'hostActionDataFeeds') && ~isempty(client.hostActionDataFeeds)
      dataFeeds = client.hostActionDataFeeds;
    end
    if ~isempty(dataText) && dataFeeds < 1
      client.hostActionDataFeeds = dataFeeds + 1;
      followup = ['The action you requested produced this result:', char(10), char(10), ...
        dataText, char(10), char(10), 'Use it now to answer the user - for example ', ...
        'analyze the profile: name the main hot spots and what to optimize. Do not only ', ...
        'tell them to open a panel.'];
      try
        client = nelson.ai.acp.internal.send(client, followup);
        acpAsyncState('set', surfaceId, client);
        active = true;
        return
      catch
        % Could not re-prompt: fall through and show the reply as is.
      end
    end
    client.hostActionDataFeeds = 0;
    acpAsyncState('set', surfaceId, client);
    % Record the completed assistant turn (markers already stripped) so the
    % conversation persists across restarts and the pane can reload it.
    nelson.ai.internal.history.append(surfaceId, 'assistant', char(text));
    nelson.ai.internal.aiAgentEmit('answer', struct('text', char(text)), surfaceId);
    return
  end
  % The agent may be waiting on a file-access permission: announce it to the UI
  % once, and auto-reject if the user does not decide in time (no hang).
  client = acpStepPermission(client, surfaceId);
  acpAsyncState('set', surfaceId, client);
  active = true;
end
%=============================================================================
function client = acpStepPermission(client, surfaceId)
  if ~isfield(client, 'pendingPermission') || isempty(client.pendingPermission)
    return
  end
  pp = client.pendingPermission;
  if ~pp.announced
    nelson.ai.internal.aiAgentEmit('permission', struct('id', pp.reqId, 'tool', pp.tool, ...
      'path', pp.path, 'title', pp.title), surfaceId);
    client.pendingPermission.announced = true;
    return
  end
  if (now - pp.time) * 86400 > 120
    client = nelson.ai.acp.internal.respondToPermission(client, false, pp.rejectOptionId);
  end
end
%=============================================================================
function acpAsyncToken(note, surfaceId)
  if ~isstruct(note) || ~isfield(note, 'params') || ~isfield(note.params, 'update')
    return
  end
  update = note.params.update;
  fragment = acpAsyncUpdateText(update);
  if ~isempty(fragment)
    nelson.ai.internal.aiAgentEmit('token', struct('text', fragment), surfaceId);
  end
  % Report a tool call as an activity line so the pane shows what the agent does.
  act = acpUpdateActivity(update);
  if ~isempty(act)
    nelson.ai.internal.aiAgentEmit('activity', act, surfaceId);
  end
end
%=============================================================================
function out = acpAsyncUpdateText(update)
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
% Terminate the ACP agent process for this surface and drop its cached client, so
% the surface's NEXT prompt spawns exactly one fresh agent (leaving the old one
% alive makes the next connect() contend on the same credentials and hang).
function disconnectAcpAsync(surfaceId)
  client = acpAsyncState('get', surfaceId);
  if ~isempty(client) && isstruct(client) && isfield(client, 'proc') && isvalid(client.proc)
    try
      client.proc.terminate();
    catch
    end
  end
  acpAsyncState('reset', surfaceId);
end
%=============================================================================
