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
    aiAgentEmit('error', struct('text', '(stopped)'), surfaceId);
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
    [done, text, client] = nelson.ai.acp.poll(client, @(note) acpAsyncToken(note, surfaceId));
    acpAsyncState('set', surfaceId, client);
  catch e
    disconnectAcpAsync(surfaceId);
    aiAgentEmit('error', struct('text', char(e.message)), surfaceId);
    return
  end
  if done
    % Keep the (now idle) client so the next prompt reuses the same agent.
    acpAsyncState('set', surfaceId, client);
    % Run any widget-piloting actions the agent requested, then show the reply
    % without the action markers.
    text = acpRunHostActions(char(text), surfaceId);
    aiAgentEmit('answer', struct('text', char(text)), surfaceId);
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
    aiAgentEmit('permission', struct('id', pp.reqId, 'tool', pp.tool, ...
      'path', pp.path, 'title', pp.title), surfaceId);
    client.pendingPermission.announced = true;
    return
  end
  if (now - pp.time) * 86400 > 120
    client = nelson.ai.acp.respondToPermission(client, false, pp.rejectOptionId);
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
    aiAgentEmit('token', struct('text', fragment), surfaceId);
  end
  % Report a tool call as an activity line so the pane shows what the agent does.
  act = acpUpdateActivity(update);
  if ~isempty(act)
    aiAgentEmit('activity', act, surfaceId);
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
