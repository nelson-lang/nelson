%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Read whatever the ACP agent has produced so far for the pending request (from
% nelson.ai.acp.send), without blocking. Returns done=true once the matching
% response has arrived (or the agent exited). onNotification is invoked for each
% streaming update. text is the reply accumulated so far.
function [done, text, client] = poll(client, onNotification)
  narginchk(1, 2);
  nargoutchk(0, 3);
  if nargin < 2
    onNotification = [];
  end
  done = true;
  text = '';
  if ~isfield(client, 'pending') || ~client.pending.active
    return
  end
  if ~isfield(client, 'proc') || ~isvalid(client.proc)
    client.pending.active = false;
    return
  end
  done = false;
  reading = true;
  while reading
    line = client.proc.readline();
    if isempty(line)
      reading = false;
      if ~client.proc.Running && client.proc.BytesAvailable == 0
        done = true;
        client.pending.active = false;
      end
    else
      [client, matched] = processLine(client, strtrim(line), onNotification);
      if matched
        reading = false;
        done = true;
      end
    end
  end
  text = client.pending.text;
end
%=============================================================================
% Handle one ndjson line. matched is true when it is the pending request's final
% response (the caller then stops reading).
function [client, matched] = processLine(client, line, onNotification)
  matched = false;
  if isempty(line)
    return
  end
  message = jsondecode(line);
  if isfield(message, 'id') && isequal(double(message.id), double(client.pending.id))
    matched = true;
    client.pending.active = false;
    if isfield(message, 'error')
      error('Nelson:ai:acpError', '%s', jsonencode(message.error));
    end
    if isfield(message, 'result')
      client.pending.text = [client.pending.text, acpContentText(message.result)];
    end
    return
  end
  if isfield(message, 'method')
    % A request from the agent carries its own id and must be answered, or the
    % agent blocks (an unanswered tool-permission request = infinite "Thinking").
    if isfield(message, 'id')
      % A permission request is forwarded to the UI for an interactive decision
      % (stored now; announced + answered by the step loop / respondToPermission).
      % Everything else is answered immediately so the agent never blocks.
      if strcmp(char(message.method), 'session/request_permission')
        client = acpStorePendingPermission(client, message);
        return
      end
      acpAnswerAgentRequest(client.proc, message);
      return
    end
    if ~isempty(onNotification)
      onNotification(message);
    end
    if isfield(message, 'params') && isfield(message.params, 'update')
      client.pending.text = [client.pending.text, acpContentText(message.params.update)];
    end
  end
end
%=============================================================================
% Store a pending permission on the client so the UI can decide, UNLESS it is
% auto-answered: mode 'full' allows everything, 'auto' allows safe requests (risky
% ones still prompt), a session-wide "always allow" allows, and a path the user
% already chose to always allow this session allows. Otherwise the request waits
% for the UI (announced by the step loop).
function client = acpStorePendingPermission(client, message)
  perm = acpExtractPermission(message);
  perm.announced = false;
  perm.time = now;
  client.pendingPermission = perm;
  mode = nelson.ai.acp.permissionMode();
  allowAll = isfield(client, 'permissionAllowAll') && client.permissionAllowAll;
  pathAllowed = acpPathRemembered(client, perm.path);
  autoAllow = strcmp(mode, 'full') || (strcmp(mode, 'auto') && ~acpIsRisky(perm)) ...
    || allowAll || pathAllowed;
  if autoAllow
    client = nelson.ai.acp.respondToPermission(client, true, perm.allowOptionId);
  end
end
%=============================================================================
