%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Start fresh: forget the persisted (shared) history AND reset the live ACP agent,
% so the next prompt begins with no memory. Clearing just the file would leave the
% running agent still remembering. Qt and web share one conversation, so both clear.
function newConversation(surfaceId)
  narginchk(0, 1);
  nargoutchk(0, 0);
  if nargin < 1
    surfaceId = 'web';
  end
  surfaceId = char(surfaceId);
  nelson.ai.internal.history.clear(surfaceId);
  resetAcpSurface(surfaceId);
end
%=============================================================================
% Terminate this surface's live ACP agent (if any) and drop its client, so the next
% prompt spawns a fresh one. No-op for a surface with no live agent.
function resetAcpSurface(surfaceId)
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
