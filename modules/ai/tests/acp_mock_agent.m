%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Minimal ACP agent used by test_acpclient: reads JSON-RPC requests on stdin
% and writes responses on stdout. Not a unit test (no test_ prefix).
while true
  line = '';
  try
    line = input('', 's');
  catch
    break
  end
  if isempty(line)
    continue
  end
  request = jsondecode(line);
  if ~isfield(request, 'method')
    continue
  end
  switch request.method
    case 'initialize'
      reply = struct('jsonrpc', '2.0', 'id', request.id, 'result', ...
        struct('protocolVersion', 1, 'agentInfo', struct('name', 'mock-agent')));
    case 'session/new'
      reply = struct('jsonrpc', '2.0', 'id', request.id, 'result', ...
        struct('sessionId', 'mock-session-1'));
    case 'session/prompt'
      note = struct('jsonrpc', '2.0', 'method', 'session/update', 'params', ...
        struct('sessionId', 'mock-session-1', 'update', struct('content', 'pong')));
      fprintf('%s\n', jsonencode(note));
      reply = struct('jsonrpc', '2.0', 'id', request.id, 'result', ...
        struct('stopReason', 'end_turn'));
    otherwise
      reply = struct('jsonrpc', '2.0', 'id', request.id, 'error', ...
        struct('code', -32601, 'message', 'Method not found'));
  end
  fprintf('%s\n', jsonencode(reply));
end
