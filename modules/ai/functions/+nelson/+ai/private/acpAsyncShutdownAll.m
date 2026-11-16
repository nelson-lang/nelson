%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Stop every surface's poll timer and terminate its claude-agent-acp process, so
% closing Nelson leaves no orphan agent and no running timer (a request left in
% "Thinking..." is torn down cleanly). Called from the ai module finish hook.
function acpAsyncShutdownAll()
  narginchk(0, 0);
  try
    pollTimers = timerfindall('Tag', 'nelson_ai_acp_poll');
    if ~isempty(pollTimers)
      stop(pollTimers);
      delete(pollTimers);
    end
  catch
  end
  surfaces = acpAsyncState('surfaces', '');
  for k = 1:numel(surfaces)
    try
      client = acpAsyncState('get', surfaces{k});
      if ~isempty(client) && isstruct(client) && isfield(client, 'proc') && isvalid(client.proc)
        client.proc.terminate();
      end
    catch
    end
    acpAsyncState('reset', surfaces{k});
  end
end
%=============================================================================
