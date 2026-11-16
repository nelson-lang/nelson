%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Open a claude-agent-acp request for ONE surface (surfaceId) WITHOUT blocking:
% reuse that surface's live agent (or connect), send the prompt, then let the
% reply be read step by step by acpAsyncStep(surfaceId). useTimer selects the
% driver: true = a Nelson timer ticks the step (web pane / Command Window);
% false = the caller drives it (the Qt dock from its own QTimer). Each surface is
% isolated, so a prompt here never blocks another surface's session.
function startAcpAsync(prompt, useTimer, surfaceId)
  narginchk(1, 3);
  if nargin < 2
    useTimer = true;
  end
  if nargin < 3
    surfaceId = 'web';
  end
  surfaceId = char(surfaceId);
  client = acpAsyncState('get', surfaceId);
  if isempty(client) || ~isAcpAsyncAlive(client)
    client = nelson.ai.acp.connect('claude-agent-acp');
  end
  aiAgentEmit('reset', struct(), surfaceId);
  __ai_interrupt_pending__(surfaceId);  % clear a stale Stop for this surface
  client = nelson.ai.acp.send(client, char(prompt));
  acpAsyncState('set', surfaceId, client);
  if useTimer
    t = timer('Period', 0.1, 'ExecutionMode', 'fixedSpacing', 'BusyMode', 'drop', ...
      'Tag', 'nelson_ai_acp_poll', 'TimerFcn', @(obj, ev) acpAsyncTick(obj, surfaceId));
    start(t);
  end
end
%=============================================================================
function acpAsyncTick(t, surfaceId)
  active = acpAsyncStep(surfaceId);
  if ~active
    try
      stop(t);
      delete(t);
    catch
    end
  end
end
%=============================================================================
function tf = isAcpAsyncAlive(client)
  tf = isstruct(client) && isfield(client, 'proc') && isvalid(client.proc);
end
%=============================================================================
