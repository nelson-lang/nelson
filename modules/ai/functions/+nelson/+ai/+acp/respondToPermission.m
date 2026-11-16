%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Answer the client's pending session/request_permission (stored on the client by
% poll) so the agent resumes: allow selects the agent's allow option, otherwise its
% reject option (or a cancelled outcome when none was offered). Clears the pending
% permission and returns the updated client. A no-op when nothing is pending.
% rememberPath (optional) records the request's path as always-allowed this session
% when the answer is allow, so the same file is not prompted again.
function client = respondToPermission(client, allow, optionId, rememberPath)
  narginchk(2, 4);
  nargoutchk(0, 1);
  if nargin < 3
    optionId = '';
  end
  if nargin < 4
    rememberPath = false;
  end
  if ~isstruct(client) || ~isfield(client, 'pendingPermission') || isempty(client.pendingPermission)
    return
  end
  pp = client.pendingPermission;
  if isempty(optionId)
    if allow
      optionId = pp.allowOptionId;
    else
      optionId = pp.rejectOptionId;
    end
  end
  if isempty(optionId)
    outcome = struct('outcome', 'cancelled');
  else
    outcome = struct('outcome', 'selected', 'optionId', char(optionId));
  end
  response = struct('jsonrpc', '2.0', 'id', pp.reqId, 'result', struct('outcome', outcome));
  if isfield(client, 'proc') && isvalid(client.proc)
    try
      client.proc.write([jsonencode(response), char(10)]);
    catch
    end
  end
  if rememberPath && allow && ~isempty(pp.path)
    client = acpRememberPath(client, pp.path);
  end
  client.pendingPermission = [];
end
%=============================================================================
