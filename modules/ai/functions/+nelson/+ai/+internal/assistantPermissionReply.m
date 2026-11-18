%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Deliver the user's decision for a surface's pending file-access permission (the
% modal replies through the host RPC with the request id and a decision). allow_*
% lets the agent proceed; allow_always also stops re-prompting this session;
% allow_always_path stops re-prompting only for this request's file; any other
% value rejects. A stale id (already resolved) is ignored.
function assistantPermissionReply(surfaceId, reqId, decision)
  narginchk(3, 3);
  nargoutchk(0, 0);
  surfaceId = char(surfaceId);
  decision = char(decision);
  client = acpAsyncState('get', surfaceId);
  if isempty(client) || ~isstruct(client) || ~isfield(client, 'pendingPermission') ...
      || isempty(client.pendingPermission)
    return
  end
  if ~isequal(double(client.pendingPermission.reqId), double(reqId))
    return
  end
  allow = startsWith(lower(decision), 'allow');
  rememberPath = false;
  if strcmpi(decision, 'allow_always')
    client.permissionAllowAll = true;
  elseif strcmpi(decision, 'allow_always_path')
    rememberPath = true;
  end
  client = nelson.ai.acp.internal.respondToPermission(client, allow, '', rememberPath);
  acpAsyncState('set', surfaceId, client);
end
%=============================================================================
