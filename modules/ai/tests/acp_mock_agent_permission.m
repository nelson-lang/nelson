%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Mock ACP agent that, on a prompt, sends a session/request_permission REQUEST and
% waits for the client to answer it before completing the turn. It lets us check
% the client answers agent requests (so a tool-permission prompt never hangs the
% run). The final reply text reports whether the answer was received. Not a unit
% test (no test_ prefix).
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
        struct('protocolVersion', 1, 'agentInfo', struct('name', 'mock-agent-perm')));
      fprintf('%s\n', jsonencode(reply));
    case 'session/new'
      reply = struct('jsonrpc', '2.0', 'id', request.id, 'result', ...
        struct('sessionId', 'mock-session-perm'));
      fprintf('%s\n', jsonencode(reply));
    case 'session/prompt'
      % Ask the client for permission to use a tool, then WAIT for its answer.
      permReq = struct('jsonrpc', '2.0', 'id', 9001, ...
        'method', 'session/request_permission', 'params', struct( ...
        'sessionId', 'mock-session-perm', 'options', ...
        {{struct('optionId', 'allow', 'name', 'Allow', 'kind', 'allow_once'), ...
        struct('optionId', 'reject', 'name', 'Reject', 'kind', 'reject_once')}}));
      fprintf('%s\n', jsonencode(permReq));
      answered = false;
      for k = 1:1000
        answerLine = '';
        try
          answerLine = input('', 's');
        catch
          answerLine = '';
        end
        if isempty(answerLine)
          continue
        end
        reply = jsondecode(answerLine);
        if isfield(reply, 'id') && isequal(double(reply.id), 9001)
          answered = true;
          break
        end
      end
      text = 'blocked';
      if answered
        text = 'permission-answered';
      end
      note = struct('jsonrpc', '2.0', 'method', 'session/update', 'params', ...
        struct('sessionId', 'mock-session-perm', 'update', struct('content', text)));
      fprintf('%s\n', jsonencode(note));
      reply = struct('jsonrpc', '2.0', 'id', request.id, 'result', ...
        struct('stopReason', 'end_turn'));
      fprintf('%s\n', jsonencode(reply));
    otherwise
      reply = struct('jsonrpc', '2.0', 'id', request.id, 'error', ...
        struct('code', -32601, 'message', 'Method not found'));
      fprintf('%s\n', jsonencode(reply));
  end
end
