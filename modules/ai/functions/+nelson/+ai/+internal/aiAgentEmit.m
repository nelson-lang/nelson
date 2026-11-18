%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Forward a live assistant progress event to ONE surface (surfaceId: 'web' /
% 'qt' / 'nflow'); the builtin routes it to that surface's sink only, so events
% no longer fan out to every pane. Silent and harmless when no host is attached
% (console, adv-cli, tests), so the agentic loop can call it unconditionally.
function aiAgentEmit(channel, payload, surfaceId)
  narginchk(2, 3);
  if nargin < 3
    surfaceId = 'web';
  end
  try
    __ai_notify__(channel, jsonencode(payload), char(surfaceId));
  catch
  end
end
%=============================================================================
