%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Send one JSON-RPC request to an ACP agent process and wait for its response.
% Notifications received before the matching response are collected and returned;
% an optional onNotification callback is also invoked for each as it arrives (used
% to stream the agent's reply live).
function [result, notifications] = acpSendRequest(proc, id, method, params, timeoutSeconds, onNotification)
  result = struct();
  notifications = {};
  if nargin < 6
    onNotification = [];
  end
  request = struct('jsonrpc', '2.0', 'id', id, 'method', method, 'params', params);
  proc.write([jsonencode(request), char(10)]);
  startTime = tic();
  while toc(startTime) < timeoutSeconds
    line = proc.readline();
    if isempty(line)
      if ~proc.Running && proc.BytesAvailable == 0
        break
      end
      pause(0.02);
      continue
    end
    line = strtrim(line);
    if isempty(line)
      continue
    end
    message = jsondecode(line);
    if isfield(message, 'id') && isequal(double(message.id), double(id))
      if isfield(message, 'error')
        error('Nelson:ai:acpError', '%s', jsonencode(message.error));
      end
      if isfield(message, 'result')
        result = message.result;
      end
      return
    end
    if isfield(message, 'method')
      % A request from the agent (method + its own id) must be answered or it
      % blocks; a bare notification (no id) is just collected/streamed.
      if isfield(message, 'id')
        acpAnswerAgentRequest(proc, message);
        continue
      end
      notifications{end + 1} = message;
      if ~isempty(onNotification)
        onNotification(message);
      end
    end
  end
  error('Nelson:ai:acpTimeout', 'No ACP response for method %s.', method);
end
%=============================================================================
